module Web.ARIA where

import Prelude

data ARIARole
  = Alert
  | Alertdialog
  | Application
  | Article
  | Banner
  | Blockquote
  | Button
  | Caption
  | Cell
  | Checkbox
  | Code
  | Columnheader
  | Combobox
  | Command
  | Complementary
  | Composite
  | Contentinfo
  | Definition
  | Deletion
  | Dialog
  | Directory
  | DocAbstract
  | DocAcknowledgments
  | DocAfterword
  | DocAppendix
  | DocBacklink
  | DocBiblioentry
  | DocBibliography
  | DocBiblioref
  | DocChapter
  | DocColophon
  | DocConclusion
  | DocCover
  | DocCredit
  | DocCredits
  | DocDedication
  | DocEndnote
  | DocEndnotes
  | DocEpigraph
  | DocEpilogue
  | DocErrata
  | DocExample
  | DocFootnote
  | DocForeword
  | DocGlossary
  | DocGlossref
  | DocIndex
  | DocIntroduction
  | DocNoteref
  | DocNotice
  | DocPagebreak
  | DocPagelist
  | DocPart
  | DocPreface
  | DocPrologue
  | DocPullquote
  | DocQna
  | DocSubtitle
  | DocTip
  | DocToc
  | Document
  | Emphasis
  | Feed
  | Figure
  | Form
  | Generic
  | Grid
  | Gridcell
  | Group
  | Heading
  | Img
  | Input
  | Insertion
  | Landmark
  | Link
  | List
  | Listbox
  | Listitem
  | Log
  | Main
  | Marquee
  | Math
  | Menu
  | Menubar
  | Menuitem
  | Menuitemcheckbox
  | Menuitemradio
  | Meter
  | Navigation
  | None
  | Note
  | Option
  | Paragraph
  | Presentation
  | Progressbar
  | Radio
  | Radiogroup
  | Range
  | Region
  | Roletype
  | Row
  | Rowgroup
  | Rowheader
  | Scrollbar
  | Search
  | Searchbox
  | Section
  | Sectionhead
  | Select
  | Separator
  | Slider
  | Spinbutton
  | Status
  | Strong
  | Structure
  | Subscript
  | Superscript
  | Switch
  | Tab
  | Table
  | Tablist
  | Tabpanel
  | Term
  | Textbox
  | Time
  | Timer
  | Toolbar
  | Tooltip
  | Tree
  | Treegrid
  | Treeitem
  | Widget
  | Window

instance Show ARIARole where
  show Alert = "alert"
  show Alertdialog = "alertdialog"
  show Application = "application"
  show Article = "article"
  show Banner = "banner"
  show Blockquote = "blockquote"
  show Button = "button"
  show Caption = "caption"
  show Cell = "cell"
  show Checkbox = "checkbox"
  show Code = "code"
  show Columnheader = "columnheader"
  show Combobox = "combobox"
  show Command = "command"
  show Complementary = "complementary"
  show Composite = "composite"
  show Contentinfo = "contentinfo"
  show Definition = "definition"
  show Deletion = "deletion"
  show Dialog = "dialog"
  show Directory = "directory"
  show DocAbstract = "doc-abstract"
  show DocAcknowledgments = "doc-acknowledgments"
  show DocAfterword = "doc-afterword"
  show DocAppendix = "doc-appendix"
  show DocBacklink = "doc-backlink"
  show DocBiblioentry = "doc-biblioentry"
  show DocBibliography = "doc-bibliography"
  show DocBiblioref = "doc-biblioref"
  show DocChapter = "doc-chapter"
  show DocColophon = "doc-colophon"
  show DocConclusion = "doc-conclusion"
  show DocCover = "doc-cover"
  show DocCredit = "doc-credit"
  show DocCredits = "doc-credits"
  show DocDedication = "doc-dedication"
  show DocEndnote = "doc-endnote"
  show DocEndnotes = "doc-endnotes"
  show DocEpigraph = "doc-epigraph"
  show DocEpilogue = "doc-epilogue"
  show DocErrata = "doc-errata"
  show DocExample = "doc-example"
  show DocFootnote = "doc-footnote"
  show DocForeword = "doc-foreword"
  show DocGlossary = "doc-glossary"
  show DocGlossref = "doc-glossref"
  show DocIndex = "doc-index"
  show DocIntroduction = "doc-introduction"
  show DocNoteref = "doc-noteref"
  show DocNotice = "doc-notice"
  show DocPagebreak = "doc-pagebreak"
  show DocPagelist = "doc-pagelist"
  show DocPart = "doc-part"
  show DocPreface = "doc-preface"
  show DocPrologue = "doc-prologue"
  show DocPullquote = "doc-pullquote"
  show DocQna = "doc-qna"
  show DocSubtitle = "doc-subtitle"
  show DocTip = "doc-tip"
  show DocToc = "doc-toc"
  show Document = "document"
  show Emphasis = "emphasis"
  show Feed = "feed"
  show Figure = "figure"
  show Form = "form"
  show Generic = "generic"
  show Grid = "grid"
  show Gridcell = "gridcell"
  show Group = "group"
  show Heading = "heading"
  show Img = "img"
  show Input = "input"
  show Insertion = "insertion"
  show Landmark = "landmark"
  show Link = "link"
  show List = "list"
  show Listbox = "listbox"
  show Listitem = "listitem"
  show Log = "log"
  show Main = "main"
  show Marquee = "marquee"
  show Math = "math"
  show Menu = "menu"
  show Menubar = "menubar"
  show Menuitem = "menuitem"
  show Menuitemcheckbox = "menuitemcheckbox"
  show Menuitemradio = "menuitemradio"
  show Meter = "meter"
  show Navigation = "navigation"
  show None = "none"
  show Note = "note"
  show Option = "option"
  show Paragraph = "paragraph"
  show Presentation = "presentation"
  show Progressbar = "progressbar"
  show Radio = "radio"
  show Radiogroup = "radiogroup"
  show Range = "range"
  show Region = "region"
  show Roletype = "roletype"
  show Row = "row"
  show Rowgroup = "rowgroup"
  show Rowheader = "rowheader"
  show Scrollbar = "scrollbar"
  show Search = "search"
  show Searchbox = "searchbox"
  show Section = "section"
  show Sectionhead = "sectionhead"
  show Select = "select"
  show Separator = "separator"
  show Slider = "slider"
  show Spinbutton = "spinbutton"
  show Status = "status"
  show Strong = "strong"
  show Structure = "structure"
  show Subscript = "subscript"
  show Superscript = "superscript"
  show Switch = "switch"
  show Tab = "tab"
  show Table = "table"
  show Tablist = "tablist"
  show Tabpanel = "tabpanel"
  show Term = "term"
  show Textbox = "textbox"
  show Time = "time"
  show Timer = "timer"
  show Toolbar = "toolbar"
  show Tooltip = "tooltip"
  show Tree = "tree"
  show Treegrid = "treegrid"
  show Treeitem = "treeitem"
  show Widget = "widget"
  show Window = "window"
