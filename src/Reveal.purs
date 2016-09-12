module Reveal where

import Prelude
import Data.Foldable (foldMap)
import Data.List
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Text.Smolder.HTML (Html)
import Text.Smolder.HTML as Html
import Text.Smolder.HTML.Attributes as Html
import Text.Smolder.Markup
import Text.Smolder.Renderer.String as Smolder

error = unsafeThrow

--------------------------------------------------------------------------------
-- * A mostly shallow embedding

type RevealOpts = { styles :: Array String, script :: String }

-- A complete slideshow
data Deck = Deck (Array Section)

-- only one level of nesting
data Section
  = Single Slide
  | Section (Array Slide)   -- A subsection, with a list of vertical slides

-- A slide is really just a piece of HTML
type Slide = Markup

-- TODO: write this as a Typed Tagless Final embedding

infix 5 vcat as  <+>
-- infix 5 hcat as  <=>
-- infix 5 nest as  <%>


vcat :: Slide -> Slide -> Slide
vcat = (*>)

-- hcat :: Slide -> Slide -> Slide
-- hcat =  error "TODO"

-- nest :: Slide -> Slide -> Slide
-- nest =  error "TODO"

title :: String -> Slide
title =  Html.h1 <<< text

subtitle :: String -> Slide
subtitle = Html.h2 <<< text

-- bullets :: String -> Slide
-- bullets =  error "TODO"

-- -- filepath
-- image :: String -> Slide
-- image =  error "TODO"


--------------------------------------------------------------------------------
-- * Compile

render :: RevealOpts -> Deck -> Html
render opts (Deck d) = contain opts $ topLevel $ foldMap renderSection d
  where
    renderSection :: Section -> Html
    renderSection (Single s) = section s
    renderSection (Section ss) = section $ foldMap section ss

    renderSlide :: Slide -> Html
    renderSlide = section

section :: Slide -> Html
section = parent "section"

topLevel :: Html -> Html
topLevel = (Html.div ! Html.className "reveal") <<<
             (Html.div ! Html.className "slides")

contain :: RevealOpts -> Html -> Html
contain opts slides = do
  Html.head $ buildStyles opts.styles
  Html.body $ slides <> script
  where
    script = (Html.script ! Html.src opts.script $ pure unit)
          <> (Html.script $ text "Reveal.initialize();")

    buildStyles = foldMap (\s -> Html.link ! (Html.rel "stylesheet" <> Html.href s))

renderString :: RevealOpts -> Deck -> String
renderString opts = Smolder.render <<< render opts

--------------------------------------------------------------------------------
-- * Test

sampleDeck :: Deck
sampleDeck = Deck
  [ Single $ title "Hello World"
  , Single $ title "Example"
                     <+>
             subtitle "This is an example"
  ]

defOpts :: RevealOpts
defOpts = { styles: ["css/reveal.css", "css/theme/white.css"]
          , script: "js/reveal.js"
          }

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ renderString defOpts sampleDeck
