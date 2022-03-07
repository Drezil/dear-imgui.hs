{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: DearImGui.Plot

Main ImPlot module, exporting the functions to make plots happen in Gui.
-}

module DearImGui.Plot
  ( -- * Context Creation and Access
    Raw.Plot.PlotContext(..)
  , Raw.Plot.createPlotContext
  , Raw.Plot.destroyPlotContext
  , Raw.Plot.getCurrentPlotContext
  , Raw.Plot.setCurrentPlotContext

    -- * Demo so you can play with all features
  , Raw.Plot.showPlotDemoWindow
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Bool
import Data.Foldable
  ( foldl' )
import Foreign
import Foreign.C
import qualified GHC.Foreign as Foreign
import System.IO
  ( utf8 )

-- dear-imgui
import DearImGui.Enums
import DearImGui.Structs
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.Plot as Raw.Plot
import qualified DearImGui.Raw.Font as Raw.Font
import qualified DearImGui.Raw.ListClipper as Raw.ListClipper

-- managed
import qualified Control.Monad.Managed as Managed

-- StateVar
import Data.StateVar
  ( HasGetter(get), HasSetter, ($=!) )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO, liftIO )

-- unliftio
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, bracket_)

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

