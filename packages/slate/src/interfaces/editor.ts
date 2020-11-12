import isPlainObject from 'is-plain-object'
import { createDraft, finishDraft, isDraft } from 'immer'
import { reverse as reverseText } from 'esrever'

import {
  Ancestor,
  Descendant,
  Element,
  Location,
  Node,
  NodeEntry,
  Operation,
  Path,
  PathRef,
  Point,
  PointRef,
  Range,
  RangeRef,
  Span,
  Text,
} from '..'
import {
  DIRTY_PATHS,
  NORMALIZING,
  PATH_REFS,
  POINT_REFS,
  RANGE_REFS,
} from '../utils/weak-maps'
import { getWordDistance, getCharacterDistance } from '../utils/string'

/**
 * The `Editor` interface stores all the state of a Slate editor. It is extended
 * by plugins that wish to add their own helpers and implement new behaviors.
 */

export interface Editor {
  children: Node[]
  selection: Range | null
  operations: Operation[]
  marks: Record<string, any> | null
  [key: string]: unknown

  // Schema-specific node behaviors.
  isInline: (element: Element) => boolean
  isVoid: (element: Element) => boolean
  normalizeNode: (entry: NodeEntry) => void
  onChange: () => void

  // Overrideable core actions.
  addMark: (key: string, value: any) => void
  apply: (operation: Operation) => void
  deleteBackward: (unit: 'character' | 'word' | 'line' | 'block') => void
  deleteForward: (unit: 'character' | 'word' | 'line' | 'block') => void
  deleteFragment: () => void
  getFragment: () => Descendant[]
  insertBreak: () => void
  insertFragment: (fragment: Node[]) => void
  insertNode: (node: Node) => void
  insertText: (text: string) => void
  removeMark: (key: string) => void
}

export const Editor = {
  /**
   * Get the ancestor above a location in the document.
   *
   * If the location is a range, it will look for a common ancestor to both
   * the anchor and focus points.
   *
   * @param editor Editor instance to search
   * @param options.at Location to start at. Defaults to editor.selection
   * @param options.match Predicate for matching the ancestor to return
   * @param options.mode Whether to return highest or lowest ancestor in the node tree. Defaults to lowest
   * @param options.voids Whether or not to include void elements in the result
   *
   * @returns A [Node, Path] tuple for the first matching ancestor
   */

  above<T extends Ancestor>(
    editor: Editor,
    options: {
      at?: Location
      match?: NodeMatch<T>
      mode?: 'highest' | 'lowest'
      voids?: boolean
    } = {}
  ): NodeEntry<T> | undefined {
    const {
      voids = false,
      mode = 'lowest',
      at = editor.selection,
      match,
    } = options

    if (!at) {
      return
    }

    const path = Editor.path(editor, at)
    const reverse = mode === 'lowest'

    for (const [n, p] of Editor.levels(editor, {
      at: path,
      voids,
      match,
      reverse,
    })) {
      if (!Text.isText(n) && !Path.equals(path, p)) {
        return [n, p]
      }
    }
  },

  /**
   * Add a custom property to the leaf text nodes in the current selection.
   *
   * If the selection is currently collapsed, the marks will be added to the
   * `editor.marks` property instead, and applied when text is inserted next.
   *
   * @param editor Editor instance that will receive the mark
   * @param key Property key that will be added to the matching leaf nodes
   * @param value Property value that will be added to the matching leaf nodes
   */

  addMark(editor: Editor, key: string, value: any): void {
    editor.addMark(key, value)
  },

  /**
   * Get the point after a location.
   *
   * @param editor Editor instance to search
   * @param at Location to start at
   * @param options.distance Number of units after the location. Defaults to 1
   * @param options.unit Distance unit. Defaults to 'offset'
   *
   * @returns Matching point
   */

  after(
    editor: Editor,
    at: Location,
    options: {
      distance?: number
      unit?: 'offset' | 'character' | 'word' | 'line' | 'block'
    } = {}
  ): Point | undefined {
    const anchor = Editor.point(editor, at, { edge: 'end' })
    const focus = Editor.end(editor, [])
    const range = { anchor, focus }
    const { distance = 1 } = options
    let d = 0
    let target

    for (const p of Editor.positions(editor, { ...options, at: range })) {
      if (d > distance) {
        break
      }

      if (d !== 0) {
        target = p
      }

      d++
    }

    return target
  },

  /**
   * Get the point before a location.
   *
   * @param editor Editor instance to search
   * @param at Location to start at
   * @param options.distance Number of units before the location. Defaults to 1
   * @param options.unit Distance unit. Defaults to 'offset'
   *
   * @returns Matching point
   */

  before(
    editor: Editor,
    at: Location,
    options: {
      distance?: number
      unit?: 'offset' | 'character' | 'word' | 'line' | 'block'
    } = {}
  ): Point | undefined {
    const anchor = Editor.start(editor, [])
    const focus = Editor.point(editor, at, { edge: 'start' })
    const range = { anchor, focus }
    const { distance = 1 } = options
    let d = 0
    let target

    for (const p of Editor.positions(editor, {
      ...options,
      at: range,
      reverse: true,
    })) {
      if (d > distance) {
        break
      }

      if (d !== 0) {
        target = p
      }

      d++
    }

    return target
  },

  /**
   * Delete content in the editor backward from the current selection.
   *
   * @param editor Editor instance
   * @param options.unit Unit to delete. Defaults to 'character'
   */

  deleteBackward(
    editor: Editor,
    options: {
      unit?: 'character' | 'word' | 'line' | 'block'
    } = {}
  ): void {
    const { unit = 'character' } = options
    editor.deleteBackward(unit)
  },

  /**
   * Delete content in the editor forward from the current selection.
   *
   * @param editor Editor instance
   * @param options.unit Unit to delete. Defaults to 'character'
   */

  deleteForward(
    editor: Editor,
    options: {
      unit?: 'character' | 'word' | 'line' | 'block'
    } = {}
  ): void {
    const { unit = 'character' } = options
    editor.deleteForward(unit)
  },

  /**
   * Delete the content in the current selection.
   *
   * @param editor Editor instance
   */

  deleteFragment(editor: Editor): void {
    editor.deleteFragment()
  },

  /**
   * Get the start and end points of a location.
   *
   * @param editor Editor instance containing the location
   * @param at Location to target
   *
   * @returns A tuple containing the start and end points
   */

  edges(editor: Editor, at: Location): [Point, Point] {
    return [Editor.start(editor, at), Editor.end(editor, at)]
  },

  /**
   * Get the end point of a location.
   *
   * @param editor Editor instance containing the location
   * @param at Location to target
   *
   * @returns The end point
   */

  end(editor: Editor, at: Location): Point {
    return Editor.point(editor, at, { edge: 'end' })
  },

  /**
   * Get the first node at a location.
   *
   * @param editor Editor instance containing the location
   * @param at Location to target
   *
   * @returns A [Node, Path] tuple for the first node
   */

  first(editor: Editor, at: Location): NodeEntry {
    const path = Editor.path(editor, at, { edge: 'start' })
    return Editor.node(editor, path)
  },

  /**
   * Get the fragment at a location.
   *
   * @param editor Editor instance containing the location
   * @param at Location to target
   *
   * @returns List of descendant nodes
   */

  fragment(editor: Editor, at: Location): Descendant[] {
    const range = Editor.range(editor, at)
    const fragment = Node.fragment(editor, range)
    return fragment
  },
  /**
   * Check if a node has block children.
   *
   * @param editor Editor instance containing the node
   * @param element Element node to check
   *
   * @returns Whether or not the node has block children.
   */

  hasBlocks(editor: Editor, element: Element): boolean {
    return element.children.some(n => Editor.isBlock(editor, n))
  },

  /**
   * Check if a node has inline and text children.
   *
   * @param editor Editor instance containing the node
   * @param element Element node to check
   *
   * @returns Whether or not the node has inline or text children
   */

  hasInlines(editor: Editor, element: Element): boolean {
    return element.children.some(
      n => Text.isText(n) || Editor.isInline(editor, n)
    )
  },

  /**
   * Check if a node has text children.
   *
   * @param editor Editor instance containing the node
   * @param element Element node to check
   *
   * @returns Whether or not all children are text nodes
   */

  hasTexts(editor: Editor, element: Element): boolean {
    return element.children.every(n => Text.isText(n))
  },

  /**
   * Insert a block break at the current selection.
   *
   * If the selection is currently expanded, it will be deleted first.
   *
   * @param editor Editor instance to insert the break into
   */

  insertBreak(editor: Editor): void {
    editor.insertBreak()
  },

  /**
   * Insert a fragment at the current selection.
   *
   * If the selection is currently expanded, it will be deleted first.
   *
   * @param editor Editor instance to insert the fragment into
   * @param fragment Fragment nodes to insert
   */

  insertFragment(editor: Editor, fragment: Node[]): void {
    editor.insertFragment(fragment)
  },

  /**
   * Insert a node at the current selection.
   *
   * If the selection is currently expanded, it will be deleted first.
   *
   * @param editor Editor instance to insert the node into
   * @param node Node to insert
   */

  insertNode(editor: Editor, node: Node): void {
    editor.insertNode(node)
  },

  /**
   * Insert text at the current selection.
   *
   * If the selection is currently expanded, it will be deleted first.
   *
   * @param editor Editor instance to insert text into
   * @param text Text to insert
   */

  insertText(editor: Editor, text: string): void {
    editor.insertText(text)
  },

  /**
   * Check if a value is a block `Element` object.
   *
   * @param editor Editor instance to check the value against
   * @param value Value to check
   *
   * @returns Whether or not the value is a block `Element` object.
   */

  isBlock(editor: Editor, value: any): value is Element {
    return Element.isElement(value) && !editor.isInline(value)
  },

  /**
   * Check if a value is an `Editor` object.
   *
   * @param value Value to check
   *
   * @returns Whether or not the value is an `Editor` object
   */

  isEditor(value: any): value is Editor {
    return (
      isPlainObject(value) &&
      typeof value.addMark === 'function' &&
      typeof value.apply === 'function' &&
      typeof value.deleteBackward === 'function' &&
      typeof value.deleteForward === 'function' &&
      typeof value.deleteFragment === 'function' &&
      typeof value.insertBreak === 'function' &&
      typeof value.insertFragment === 'function' &&
      typeof value.insertNode === 'function' &&
      typeof value.insertText === 'function' &&
      typeof value.isInline === 'function' &&
      typeof value.isVoid === 'function' &&
      typeof value.normalizeNode === 'function' &&
      typeof value.onChange === 'function' &&
      typeof value.removeMark === 'function' &&
      (value.marks === null || isPlainObject(value.marks)) &&
      (value.selection === null || Range.isRange(value.selection)) &&
      Node.isNodeList(value.children) &&
      Operation.isOperationList(value.operations)
    )
  },

  /**
   * Check if a point is the end point of a location.
   *
   * @param editor Editor containing the point to check
   * @param at Location to check
   */

  isEnd(editor: Editor, point: Point, at: Location): boolean {
    const end = Editor.end(editor, at)
    return Point.equals(point, end)
  },

  /**
   * Check if a point is an edge of a location.
   *
   * @param editor Editor containing the point and location
   * @param point Point to check
   * @param at Location to check
   *
   * @returns Whether or not the point is an edge of the given location
   */

  isEdge(editor: Editor, point: Point, at: Location): boolean {
    return Editor.isStart(editor, point, at) || Editor.isEnd(editor, point, at)
  },

  /**
   * Check if an element is empty, accounting for void nodes.
   *
   * @param editor Editor containing the element
   * @param element Element node to check
   *
   * @returns Whether or not the element is empty
   */

  isEmpty(editor: Editor, element: Element): boolean {
    const { children } = element
    const [first] = children
    return (
      children.length === 0 ||
      (children.length === 1 &&
        Text.isText(first) &&
        first.text === '' &&
        !editor.isVoid(element))
    )
  },

  /**
   * Check if a value is an inline `Element` object.
   *
   * @param editor Editor to check the value against
   * @param value Value to check
   *
   * @returns Whether or not the value is an inline `Element` object
   */

  isInline(editor: Editor, value: any): value is Element {
    return Element.isElement(value) && editor.isInline(value)
  },

  /**
   * Check if the editor is currently normalizing after each operation.
   *
   * @param editor Editor instance to check
   *
   * @returns Whether or not the editor is currently normalizing
   */

  isNormalizing(editor: Editor): boolean {
    const isNormalizing = NORMALIZING.get(editor)
    return isNormalizing === undefined ? true : isNormalizing
  },

  /**
   * Check if a point is the start point of a location.
   *
   * @param editor Editor containing the point and location
   * @param point Point to check
   * @param at Location to check
   *
   * @returns Whether or not the point is the start of the given location
   */

  isStart(editor: Editor, point: Point, at: Location): boolean {
    // PERF: If the offset isn't `0` we know it's not the start.
    if (point.offset !== 0) {
      return false
    }

    const start = Editor.start(editor, at)
    return Point.equals(point, start)
  },

  /**
   * Check if a value is a void `Element` object.
   *
   * @param editor Editor to check the value against
   * @param value Value to check
   *
   * @returns Whether or not the value is a void `Element` object
   */

  isVoid(editor: Editor, value: any): value is Element {
    return Element.isElement(value) && editor.isVoid(value)
  },

  /**
   * Get the last node at a location.
   *
   * @param editor Editor containing the location
   * @param at Location to check
   *
   * @returns A [Node, Path] tuple for the last node at the given location
   */

  last(editor: Editor, at: Location): NodeEntry {
    const path = Editor.path(editor, at, { edge: 'end' })
    return Editor.node(editor, path)
  },

  /**
   * Get the leaf text node at a location.
   *
   * @param editor Editor containing the location
   * @param at Location to check
   * @param options.depth Max path depth to search
   * @param options.edge If `at` is a `Range`, return the 'start' or 'end' leaf
   *  node
   *
   * @returns A [Node, Path] tuple for the leaf text node at the given location.
   *  If the path matching the given options is not a Text node, an error will be
   *  thrown.
   */

  leaf(
    editor: Editor,
    at: Location,
    options: {
      depth?: number
      edge?: 'start' | 'end'
    } = {}
  ): NodeEntry<Text> {
    const path = Editor.path(editor, at, options)
    const node = Node.leaf(editor, path)
    return [node, path]
  },

  /**
   * Iterate through all of the levels at a location.
   *
   * The iteration starts at the editor root downward to the
   * node corresponding to the `at` option unless `reverse` is
   * true in which case the interation will go from bottom to top.
   * If no `at` value is provided, the current editor selection
   * will be used. If `at` is a range, the closest common root will
   * be targetted.
   *
   * @param editor Editor instance to traverse
   * @param options.at Target location. Will determine the last yielded
   *  entry or in the case where `reverse ` is true, it will determine
   *  the first. If `at` is a range, the closest common root will be targetted.
   * @param options.match Predicate function to filter yielded results
   * @param options.reverse If set to true, reverses the order of yielded nodes
   * @param options.voids Whether or not to include void nodes. Defaults to false
   *
   * @yields [Node, Path] entries
   */

  *levels<T extends Node>(
    editor: Editor,
    options: {
      at?: Location
      match?: NodeMatch<T>
      reverse?: boolean
      voids?: boolean
    } = {}
  ): Generator<NodeEntry<T>, void, undefined> {
    const { at = editor.selection, reverse = false, voids = false } = options
    let { match } = options

    if (match == null) {
      match = () => true
    }

    if (!at) {
      return
    }

    const levels: NodeEntry<T>[] = []
    const path = Editor.path(editor, at)

    for (const [n, p] of Node.levels(editor, path)) {
      if (!match(n)) {
        continue
      }

      levels.push([n, p])

      if (!voids && Editor.isVoid(editor, n)) {
        break
      }
    }

    if (reverse) {
      levels.reverse()
    }

    yield* levels
  },

  /**
   * Get the marks that would be added to text at the current selection.
   *
   * @param editor Editor instance containing the marks
   *
   * @returns a marks object containing key / value pairs
   */

  marks(editor: Editor): Record<string, any> | null {
    const { marks, selection } = editor

    if (!selection) {
      return null
    }

    if (marks) {
      return marks
    }

    if (Range.isExpanded(selection)) {
      const [match] = Editor.nodes(editor, { match: Text.isText })

      if (match) {
        const [node] = match as NodeEntry<Text>
        const { text, ...rest } = node
        return rest
      } else {
        return {}
      }
    }

    const { anchor } = selection
    const { path } = anchor
    let [node] = Editor.leaf(editor, path)

    if (anchor.offset === 0) {
      const prev = Editor.previous(editor, { at: path, match: Text.isText })
      const block = Editor.above(editor, {
        match: n => Editor.isBlock(editor, n),
      })

      if (prev && block) {
        const [prevNode, prevPath] = prev
        const [, blockPath] = block

        if (Path.isAncestor(blockPath, prevPath)) {
          node = prevNode as Text
        }
      }
    }

    const { text, ...rest } = node
    return rest
  },

  /**
   * Get the matching node in the branch of the document after a location.
   *
   * An Editor.nodes() tree traversal will be defined starting with the last node
   * of the `at` location and ending with the last node of the `editor`. The
   * traversal can be further constrained by the `mode` and `voids` options.
   *
   * @param editor Editor containing the node
   * @param options.at Starting location (defaults to editor.selection). Node
   *  traversal will start at the last node of the `at` location and end at the
   *  last node of the `editor` until a match is found. If `at` is a `Path`, the
   *  node traversal will be constrained to direct sibling nodes in the current
   *  subtree
   * @param options.match Predicate function that filters the nodes that get traversed
   * @param options.mode Optionally constrain the node traversal to a level of the tree
   * @param options.voids Optionally include void nodes in the traversal
   *
   * @returns [Node, Path] entry for next matching node
   */

  next<T extends Node>(
    editor: Editor,
    options: {
      at?: Location
      match?: NodeMatch<T>
      mode?: 'all' | 'highest' | 'lowest'
      voids?: boolean
    } = {}
  ): NodeEntry<T> | undefined {
    const { mode = 'lowest', voids = false } = options
    let { match, at = editor.selection } = options

    if (!at) {
      return
    }

    const [, from] = Editor.last(editor, at)
    const [, to] = Editor.last(editor, [])
    const span: Span = [from, to]

    if (Path.isPath(at) && at.length === 0) {
      throw new Error(`Cannot get the next node from the root node!`)
    }

    if (match == null) {
      if (Path.isPath(at)) {
        const [parent] = Editor.parent(editor, at)
        match = n => parent.children.includes(n)
      } else {
        match = () => true
      }
    }

    const [, next] = Editor.nodes(editor, { at: span, match, mode, voids })
    return next
  },

  /**
   * Get the node at a location.
   *
   * @param at Location to check
   * @param options.depth Max path depth to search
   * @param options.edge Return the Path to the start or end leaf node for the
   *  tree determined by the `at` and `options.depth` args
   *
   * @returns A [Node, Path] tuple for the matching node
   */

  node(
    editor: Editor,
    at: Location,
    options: {
      depth?: number
      edge?: 'start' | 'end'
    } = {}
  ): NodeEntry {
    const path = Editor.path(editor, at, options)
    const node = Node.get(editor, path)
    return [node, path]
  },

  /**
   * Iterate through all of the nodes in the Editor.
   *
   * The at + match options determine an initial node set to yield.
   * The mode, universal, and voids options can be used to apply additional
   * constraints to determine which nodes are yielded. The reverse option determines
   * the order in which child nodes are returned.
   *
   * @param editor Editor containing the nodes to iterate
   * @param options.at Location to constrain the list of nodes to. Defaults to editor.selection
   * @param options.match Predicate function to filter the list of yielded nodes
   * @param options.mode Further constrains the yielded node set to a single level of the tree
   * @param options.universal Setting this to true means that all Text nodes that
   *  are included in the at + mode constraints must also satisfy the match constraint
   * @param options.reverse Reverse the order of nodes yielded across the current level
   * @param options.voids Whether or not to include void elements in the result
   *
   * @yields [Node, Path] tuples matching the given constraints
   */

  *nodes<T extends Node>(
    editor: Editor,
    options: {
      at?: Location | Span
      match?: NodeMatch<T>
      mode?: 'all' | 'highest' | 'lowest'
      universal?: boolean
      reverse?: boolean
      voids?: boolean
    } = {}
  ): Generator<NodeEntry<T>, void, undefined> {
    const {
      at = editor.selection,
      mode = 'all',
      universal = false,
      reverse = false,
      voids = false,
    } = options
    let { match } = options

    if (!match) {
      match = () => true
    }

    if (!at) {
      return
    }

    let from
    let to

    if (Span.isSpan(at)) {
      from = at[0]
      to = at[1]
    } else {
      const first = Editor.path(editor, at, { edge: 'start' })
      const last = Editor.path(editor, at, { edge: 'end' })
      from = reverse ? last : first
      to = reverse ? first : last
    }

    const nodeEntries = Node.nodes(editor, {
      reverse,
      from,
      to,
      pass: ([n]) => (voids ? false : Editor.isVoid(editor, n)),
    })

    const matches: NodeEntry<T>[] = []
    let hit: NodeEntry<T> | undefined

    for (const [node, path] of nodeEntries) {
      const isLower = hit && Path.compare(path, hit[1]) === 0

      // In highest mode any node lower than the last hit is not a match.
      if (mode === 'highest' && isLower) {
        continue
      }

      if (!match(node)) {
        // If we've arrived at a leaf text node that is not lower than the last
        // hit, then we've found a branch that doesn't include a match, which
        // means the match is not universal.
        if (universal && !isLower && Text.isText(node)) {
          return
        } else {
          continue
        }
      }

      // If there's a match and it's lower than the last, update the hit.
      if (mode === 'lowest' && isLower) {
        hit = [node, path]
        continue
      }

      // In lowest mode we emit the last hit, once it's guaranteed lowest.
      const emit: NodeEntry<T> | undefined =
        mode === 'lowest' ? hit : [node, path]

      if (emit) {
        if (universal) {
          matches.push(emit)
        } else {
          yield emit
        }
      }

      hit = [node, path]
    }

    // Since lowest is always emitting one behind, catch up at the end.
    if (mode === 'lowest' && hit) {
      if (universal) {
        matches.push(hit)
      } else {
        yield hit
      }
    }

    // Universal defers to ensure that the match occurs in every branch, so we
    // yield all of the matches after iterating.
    if (universal) {
      yield* matches
    }
  },
  /**
   * Normalize any dirty objects in the editor.
   *
   * @param editor Editor to normalize
   * @param options.force
   */

  normalize(
    editor: Editor,
    options: {
      force?: boolean
    } = {}
  ) {
    const { force = false } = options
    const getDirtyPaths = (editor: Editor) => {
      return DIRTY_PATHS.get(editor) || []
    }

    if (!Editor.isNormalizing(editor)) {
      return
    }

    if (force) {
      const allPaths = Array.from(Node.nodes(editor), ([, p]) => p)
      DIRTY_PATHS.set(editor, allPaths)
    }

    if (getDirtyPaths(editor).length === 0) {
      return
    }

    Editor.withoutNormalizing(editor, () => {
      const max = getDirtyPaths(editor).length * 42 // HACK: better way?
      let m = 0

      while (getDirtyPaths(editor).length !== 0) {
        if (m > max) {
          throw new Error(`
            Could not completely normalize the editor after ${max} iterations! This is usually due to incorrect normalization logic that leaves a node in an invalid state.
          `)
        }

        const path = getDirtyPaths(editor).pop()!
        const entry = Editor.node(editor, path)
        editor.normalizeNode(entry)
        m++
      }
    })
  },

  /**
   * Get the parent node of a location.
   */

  parent(
    editor: Editor,
    at: Location,
    options: {
      depth?: number
      edge?: 'start' | 'end'
    } = {}
  ): NodeEntry<Ancestor> {
    const path = Editor.path(editor, at, options)
    const parentPath = Path.parent(path)
    const entry = Editor.node(editor, parentPath)
    return entry as NodeEntry<Ancestor>
  },

  /**
   * Get the path of a location.
   *
   * @param editor Editor containing the location to check
   * @param at Location to check
   * @param options.depth Max path depth to search
   * @param options.edge Return the Path to the start or end leaf node for the
   *  tree determined by the `at` and `options.depth` args
   *
   * @returns The path matching the given criteria
   */

  path(
    editor: Editor,
    at: Location,
    options: {
      depth?: number
      edge?: 'start' | 'end'
    } = {}
  ): Path {
    const { depth, edge } = options

    if (Path.isPath(at)) {
      if (edge === 'start') {
        const [, firstPath] = Node.first(editor, at)
        at = firstPath
      } else if (edge === 'end') {
        const [, lastPath] = Node.last(editor, at)
        at = lastPath
      }
    }

    if (Range.isRange(at)) {
      if (edge === 'start') {
        at = Range.start(at)
      } else if (edge === 'end') {
        at = Range.end(at)
      } else {
        at = Path.common(at.anchor.path, at.focus.path)
      }
    }

    if (Point.isPoint(at)) {
      at = at.path
    }

    if (depth != null) {
      at = at.slice(0, depth)
    }

    return at
  },

  /**
   * Create a mutable ref for a `Path` object, which will stay in sync as new
   * operations are applied to the editor.
   */

  pathRef(
    editor: Editor,
    path: Path,
    options: {
      affinity?: 'backward' | 'forward' | null
    } = {}
  ): PathRef {
    const { affinity = 'forward' } = options
    const ref: PathRef = {
      current: path,
      affinity,
      unref() {
        const { current } = ref
        const pathRefs = Editor.pathRefs(editor)
        pathRefs.delete(ref)
        ref.current = null
        return current
      },
    }

    const refs = Editor.pathRefs(editor)
    refs.add(ref)
    return ref
  },

  /**
   * Get the set of currently tracked path refs of the editor.
   */

  pathRefs(editor: Editor): Set<PathRef> {
    let refs = PATH_REFS.get(editor)

    if (!refs) {
      refs = new Set()
      PATH_REFS.set(editor, refs)
    }

    return refs
  },

  /**
   * Get the start or end point of a location.
   */

  point(
    editor: Editor,
    at: Location,
    options: {
      edge?: 'start' | 'end'
    } = {}
  ): Point {
    const { edge = 'start' } = options

    if (Path.isPath(at)) {
      let path

      if (edge === 'end') {
        const [, lastPath] = Node.last(editor, at)
        path = lastPath
      } else {
        const [, firstPath] = Node.first(editor, at)
        path = firstPath
      }

      const node = Node.get(editor, path)

      if (!Text.isText(node)) {
        throw new Error(
          `Cannot get the ${edge} point in the node at path [${at}] because it has no ${edge} text node.`
        )
      }

      return { path, offset: edge === 'end' ? node.text.length : 0 }
    }

    if (Range.isRange(at)) {
      const [start, end] = Range.edges(at)
      return edge === 'start' ? start : end
    }

    return at
  },

  /**
   * Create a mutable ref for a `Point` object, which will stay in sync as new
   * operations are applied to the editor.
   */

  pointRef(
    editor: Editor,
    point: Point,
    options: {
      affinity?: 'backward' | 'forward' | null
    } = {}
  ): PointRef {
    const { affinity = 'forward' } = options
    const ref: PointRef = {
      current: point,
      affinity,
      unref() {
        const { current } = ref
        const pointRefs = Editor.pointRefs(editor)
        pointRefs.delete(ref)
        ref.current = null
        return current
      },
    }

    const refs = Editor.pointRefs(editor)
    refs.add(ref)
    return ref
  },

  /**
   * Get the set of currently tracked point refs of the editor.
   */

  pointRefs(editor: Editor): Set<PointRef> {
    let refs = POINT_REFS.get(editor)

    if (!refs) {
      refs = new Set()
      POINT_REFS.set(editor, refs)
    }

    return refs
  },

  /**
   * Iterate through all of the positions in the document where a `Point` can be
   * placed.
   *
   * By default it will move forward by individual offsets at a time,  but you
   * can pass the `unit: 'character'` option to moved forward one character, word,
   * or line at at time.
   *
   * Note: void nodes are treated as a single point, and iteration will not
   * happen inside their content.
   */

  *positions(
    editor: Editor,
    options: {
      at?: Location
      unit?: 'offset' | 'character' | 'word' | 'line' | 'block'
      reverse?: boolean
    } = {}
  ): Generator<Point, void, undefined> {
    const { at = editor.selection, unit = 'offset', reverse = false } = options

    if (!at) {
      return
    }

    const range = Editor.range(editor, at)
    const [start, end] = Range.edges(range)
    const first = reverse ? end : start
    let string = ''
    let available = 0
    let offset = 0
    let distance: number | null = null
    let isNewBlock = false

    const advance = () => {
      if (distance == null) {
        if (unit === 'character') {
          distance = getCharacterDistance(string)
        } else if (unit === 'word') {
          distance = getWordDistance(string)
        } else if (unit === 'line' || unit === 'block') {
          distance = string.length
        } else {
          distance = 1
        }

        string = string.slice(distance)
      }

      // Add or substract the offset.
      offset = reverse ? offset - distance : offset + distance
      // Subtract the distance traveled from the available text.
      available = available - distance!
      // If the available had room to spare, reset the distance so that it will
      // advance again next time. Otherwise, set it to the overflow amount.
      distance = available >= 0 ? null : 0 - available
    }

    for (const [node, path] of Editor.nodes(editor, { at, reverse })) {
      if (Element.isElement(node)) {
        // Void nodes are a special case, since we don't want to iterate over
        // their content. We instead always just yield their first point.
        if (editor.isVoid(node)) {
          yield Editor.start(editor, path)
          continue
        }

        if (editor.isInline(node)) {
          continue
        }

        if (Editor.hasInlines(editor, node)) {
          const e = Path.isAncestor(path, end.path)
            ? end
            : Editor.end(editor, path)
          const s = Path.isAncestor(path, start.path)
            ? start
            : Editor.start(editor, path)

          const text = Editor.string(editor, { anchor: s, focus: e })
          string = reverse ? reverseText(text) : text
          isNewBlock = true
        }
      }

      if (Text.isText(node)) {
        const isFirst = Path.equals(path, first.path)
        available = node.text.length
        offset = reverse ? available : 0

        if (isFirst) {
          available = reverse ? first.offset : available - first.offset
          offset = first.offset
        }

        if (isFirst || isNewBlock || unit === 'offset') {
          yield { path, offset }
        }

        while (true) {
          // If there's no more string, continue to the next block.
          if (string === '') {
            break
          } else {
            advance()
          }

          // If the available space hasn't overflow, we have another point to
          // yield in the current text node.
          if (available >= 0) {
            yield { path, offset }
          } else {
            break
          }
        }

        isNewBlock = false
      }
    }
  },

  /**
   * Get the matching node in the branch of the document before a location.
   */

  previous<T extends Node>(
    editor: Editor,
    options: {
      at?: Location
      match?: NodeMatch<T>
      mode?: 'all' | 'highest' | 'lowest'
      voids?: boolean
    } = {}
  ): NodeEntry<T> | undefined {
    const { mode = 'lowest', voids = false } = options
    let { match, at = editor.selection } = options

    if (!at) {
      return
    }

    const [, from] = Editor.first(editor, at)
    const [, to] = Editor.first(editor, [])
    const span: Span = [from, to]

    if (Path.isPath(at) && at.length === 0) {
      throw new Error(`Cannot get the previous node from the root node!`)
    }

    if (match == null) {
      if (Path.isPath(at)) {
        const [parent] = Editor.parent(editor, at)
        match = n => parent.children.includes(n)
      } else {
        match = () => true
      }
    }

    const [, previous] = Editor.nodes(editor, {
      reverse: true,
      at: span,
      match,
      mode,
      voids,
    })

    return previous
  },

  /**
   * Get a range of a location.
   */

  range(editor: Editor, at: Location, to?: Location): Range {
    if (Range.isRange(at) && !to) {
      return at
    }

    const start = Editor.start(editor, at)
    const end = Editor.end(editor, to || at)
    return { anchor: start, focus: end }
  },

  /**
   * Create a mutable ref for a `Range` object, which will stay in sync as new
   * operations are applied to the editor.
   */

  rangeRef(
    editor: Editor,
    range: Range,
    options: {
      affinity?: 'backward' | 'forward' | 'outward' | 'inward' | null
    } = {}
  ): RangeRef {
    const { affinity = 'forward' } = options
    const ref: RangeRef = {
      current: range,
      affinity,
      unref() {
        const { current } = ref
        const rangeRefs = Editor.rangeRefs(editor)
        rangeRefs.delete(ref)
        ref.current = null
        return current
      },
    }

    const refs = Editor.rangeRefs(editor)
    refs.add(ref)
    return ref
  },

  /**
   * Get the set of currently tracked range refs of the editor.
   */

  rangeRefs(editor: Editor): Set<RangeRef> {
    let refs = RANGE_REFS.get(editor)

    if (!refs) {
      refs = new Set()
      RANGE_REFS.set(editor, refs)
    }

    return refs
  },

  /**
   * Remove a custom property from all of the leaf text nodes in the current
   * selection.
   *
   * If the selection is currently collapsed, the removal will be stored on
   * `editor.marks` and applied to the text inserted next.
   *
   * @param editor Editor instance that will have the mark removed
   * @param key Property key that will be removed
   */

  removeMark(editor: Editor, key: string): void {
    editor.removeMark(key)
  },

  /**
   * Get the start point of a location.
   */

  start(editor: Editor, at: Location): Point {
    return Editor.point(editor, at, { edge: 'start' })
  },

  /**
   * Get the text string content of a location.
   *
   * Note: the text of void nodes is presumed to be an empty string, regardless
   * of what their actual content is.
   */

  string(editor: Editor, at: Location): string {
    const range = Editor.range(editor, at)
    const [start, end] = Range.edges(range)
    let text = ''

    for (const [node, path] of Editor.nodes(editor, {
      at: range,
      match: Text.isText,
    })) {
      let t = node.text

      if (Path.equals(path, end.path)) {
        t = t.slice(0, end.offset)
      }

      if (Path.equals(path, start.path)) {
        t = t.slice(start.offset)
      }

      text += t
    }

    return text
  },

  /**
   * Convert a range into a non-hanging one.
   */

  unhangRange(
    editor: Editor,
    range: Range,
    options: {
      voids?: boolean
    } = {}
  ): Range {
    const { voids = false } = options
    let [start, end] = Range.edges(range)

    // PERF: exit early if we can guarantee that the range isn't hanging.
    if (start.offset !== 0 || end.offset !== 0 || Range.isCollapsed(range)) {
      return range
    }

    const endBlock = Editor.above(editor, {
      at: end,
      match: n => Editor.isBlock(editor, n),
    })
    const blockPath = endBlock ? endBlock[1] : []
    const first = Editor.start(editor, [])
    const before = { anchor: first, focus: end }
    let skip = true

    for (const [node, path] of Editor.nodes(editor, {
      at: before,
      match: Text.isText,
      reverse: true,
      voids,
    })) {
      if (skip) {
        skip = false
        continue
      }

      if (node.text !== '' || Path.isBefore(path, blockPath)) {
        end = { path, offset: node.text.length }
        break
      }
    }

    return { anchor: start, focus: end }
  },

  /**
   * Match a void node in the current branch of the editor.
   */

  void(
    editor: Editor,
    options: {
      at?: Location
      mode?: 'highest' | 'lowest'
      voids?: boolean
    } = {}
  ): NodeEntry<Element> | undefined {
    return Editor.above(editor, {
      ...options,
      match: n => Editor.isVoid(editor, n),
    })
  },

  /**
   * Call a function, deferring normalization until after it completes.
   */

  withoutNormalizing(editor: Editor, fn: () => void): void {
    const value = Editor.isNormalizing(editor)
    NORMALIZING.set(editor, false)
    fn()
    NORMALIZING.set(editor, value)
    Editor.normalize(editor)
  },
}

/**
 * A helper type for narrowing matched nodes with a predicate.
 */

type NodeMatch<T extends Node> =
  | ((node: Node) => node is T)
  | ((node: Node) => boolean)
