module Reveal where

import Prelude
import Data.Foldable (foldMap)
import Data.List
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Text.Smolder.HTML as Html
import Text.Smolder.HTML.Attributes as Html
import Text.Smolder.Markup
import Text.Smolder.Renderer.String as Smolder

error = unsafeThrow

data RevealOpts = RevealOpts { style :: String }

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

render :: Deck -> String
render (Deck d) = foldMap renderSection d
  where
    renderSection (Single s) = renderSlide s
    renderSection (Section ss) = foldMap renderSlide ss

    renderSlide = Smolder.render <<< parent "section"


sampleDeck :: Deck
sampleDeck = Deck
  [ Single $ title "Hello World"
  , Single $ title "Example"
                     <+>
             subtitle "This is an example"
  ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ render sampleDeck
