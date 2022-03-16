module Web.ARIA where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , fromString
  , toString
  )

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
  | Mark
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

-- Note: We use equality on show as a derive instance warns with the following:
--   An exhaustivity check was abandoned due to too many possible cases.
--   You may want to decompose your data types into smaller types.of too
instance Eq ARIARole where
  eq a b = show a == show b

instance DecodeJson ARIARole where
  decodeJson json = case toString json of
    Just "alert" -> Right Alert
    Just "alertdialog" -> Right Alertdialog
    Just "application" -> Right Application
    Just "article" -> Right Article
    Just "banner" -> Right Banner
    Just "blockquote" -> Right Blockquote
    Just "button" -> Right Button
    Just "caption" -> Right Caption
    Just "cell" -> Right Cell
    Just "checkbox" -> Right Checkbox
    Just "code" -> Right Code
    Just "columnheader" -> Right Columnheader
    Just "combobox" -> Right Combobox
    Just "command" -> Right Command
    Just "complementary" -> Right Complementary
    Just "composite" -> Right Composite
    Just "contentinfo" -> Right Contentinfo
    Just "definition" -> Right Definition
    Just "deletion" -> Right Deletion
    Just "dialog" -> Right Dialog
    Just "directory" -> Right Directory
    Just "doc-abstract" -> Right DocAbstract
    Just "doc-acknowledgments" -> Right DocAcknowledgments
    Just "doc-afterword" -> Right DocAfterword
    Just "doc-appendix" -> Right DocAppendix
    Just "doc-backlink" -> Right DocBacklink
    Just "doc-biblioentry" -> Right DocBiblioentry
    Just "doc-bibliography" -> Right DocBibliography
    Just "doc-biblioref" -> Right DocBiblioref
    Just "doc-chapter" -> Right DocChapter
    Just "doc-colophon" -> Right DocColophon
    Just "doc-conclusion" -> Right DocConclusion
    Just "doc-cover" -> Right DocCover
    Just "doc-credit" -> Right DocCredit
    Just "doc-credits" -> Right DocCredits
    Just "doc-dedication" -> Right DocDedication
    Just "doc-endnote" -> Right DocEndnote
    Just "doc-endnotes" -> Right DocEndnotes
    Just "doc-epigraph" -> Right DocEpigraph
    Just "doc-epilogue" -> Right DocEpilogue
    Just "doc-errata" -> Right DocErrata
    Just "doc-example" -> Right DocExample
    Just "doc-footnote" -> Right DocFootnote
    Just "doc-foreword" -> Right DocForeword
    Just "doc-glossary" -> Right DocGlossary
    Just "doc-glossref" -> Right DocGlossref
    Just "doc-index" -> Right DocIndex
    Just "doc-introduction" -> Right DocIntroduction
    Just "doc-noteref" -> Right DocNoteref
    Just "doc-notice" -> Right DocNotice
    Just "doc-pagebreak" -> Right DocPagebreak
    Just "doc-pagelist" -> Right DocPagelist
    Just "doc-part" -> Right DocPart
    Just "doc-preface" -> Right DocPreface
    Just "doc-prologue" -> Right DocPrologue
    Just "doc-pullquote" -> Right DocPullquote
    Just "doc-qna" -> Right DocQna
    Just "doc-subtitle" -> Right DocSubtitle
    Just "doc-tip" -> Right DocTip
    Just "doc-toc" -> Right DocToc
    Just "document" -> Right Document
    Just "emphasis" -> Right Emphasis
    Just "feed" -> Right Feed
    Just "figure" -> Right Figure
    Just "form" -> Right Form
    Just "generic" -> Right Generic
    Just "grid" -> Right Grid
    Just "gridcell" -> Right Gridcell
    Just "group" -> Right Group
    Just "heading" -> Right Heading
    Just "img" -> Right Img
    Just "input" -> Right Input
    Just "insertion" -> Right Insertion
    Just "landmark" -> Right Landmark
    Just "link" -> Right Link
    Just "list" -> Right List
    Just "listbox" -> Right Listbox
    Just "listitem" -> Right Listitem
    Just "log" -> Right Log
    Just "main" -> Right Main
    Just "mark" -> Right Mark
    Just "marquee" -> Right Marquee
    Just "math" -> Right Math
    Just "menu" -> Right Menu
    Just "menubar" -> Right Menubar
    Just "menuitem" -> Right Menuitem
    Just "menuitemcheckbox" -> Right Menuitemcheckbox
    Just "menuitemradio" -> Right Menuitemradio
    Just "meter" -> Right Meter
    Just "navigation" -> Right Navigation
    Just "none" -> Right None
    Just "note" -> Right Note
    Just "option" -> Right Option
    Just "paragraph" -> Right Paragraph
    Just "presentation" -> Right Presentation
    Just "progressbar" -> Right Progressbar
    Just "radio" -> Right Radio
    Just "radiogroup" -> Right Radiogroup
    Just "range" -> Right Range
    Just "region" -> Right Region
    Just "roletype" -> Right Roletype
    Just "row" -> Right Row
    Just "rowgroup" -> Right Rowgroup
    Just "rowheader" -> Right Rowheader
    Just "scrollbar" -> Right Scrollbar
    Just "search" -> Right Search
    Just "searchbox" -> Right Searchbox
    Just "section" -> Right Section
    Just "sectionhead" -> Right Sectionhead
    Just "select" -> Right Select
    Just "separator" -> Right Separator
    Just "slider" -> Right Slider
    Just "spinbutton" -> Right Spinbutton
    Just "status" -> Right Status
    Just "strong" -> Right Strong
    Just "structure" -> Right Structure
    Just "subscript" -> Right Subscript
    Just "superscript" -> Right Superscript
    Just "switch" -> Right Switch
    Just "tab" -> Right Tab
    Just "table" -> Right Table
    Just "tablist" -> Right Tablist
    Just "tabpanel" -> Right Tabpanel
    Just "term" -> Right Term
    Just "textbox" -> Right Textbox
    Just "time" -> Right Time
    Just "timer" -> Right Timer
    Just "toolbar" -> Right Toolbar
    Just "tooltip" -> Right Tooltip
    Just "tree" -> Right Tree
    Just "treegrid" -> Right Treegrid
    Just "treeitem" -> Right Treeitem
    Just "widget" -> Right Widget
    Just "window" -> Right Window
    _ -> Left $ TypeMismatch "ARIARole"

instance EncodeJson ARIARole where
  encodeJson = fromString <<< show

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
  show Mark = "mark"
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
