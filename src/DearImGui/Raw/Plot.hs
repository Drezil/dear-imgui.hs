{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module: DearImGui.Raw.Plot

Main ImPlot Raw module.
-}
module DearImGui.Raw.Plot 
  ( PlotContext(..)
  , createPlotContext
  , destroyPlotContext
  , getCurrentPlotContext
  , setCurrentPlotContext

  , showPlotDemoWindow
  ) where

-- base
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign
import Foreign.C
import System.IO.Unsafe
  ( unsafePerformIO )

-- dear-imgui
import DearImGui.Context
  ( imguiContext, implotContext )
import DearImGui.Enums
import DearImGui.Structs
import DearImGui.Raw.DrawList (DrawList(..))

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext <> implotContext)
C.include "imgui.h"
C.include "implot.h"
Cpp.using "namespace ImPlot"


-- | Wraps @ImPlotContext*@.
newtype PlotContext = PlotContext (Ptr ImPlotContext)


-- | Wraps @ImPlot::CreateContext()@.
createPlotContext :: (MonadIO m) => m PlotContext
createPlotContext = liftIO do
  PlotContext <$> [C.exp| ImPlotContext* { CreateContext() } |]

-- | Wraps @ImPlot::DestroyPlotContext()@.
destroyPlotContext :: (MonadIO m) => PlotContext -> m ()
destroyPlotContext (PlotContext contextPtr) = liftIO do
  [C.exp| void { DestroyContext($(ImPlotContext* contextPtr)); } |]

-- | Wraps @ImPlot::GetCurrentPlotContext()@.
getCurrentPlotContext :: MonadIO m => m PlotContext
getCurrentPlotContext = liftIO do
  PlotContext <$> [C.exp| ImPlotContext* { GetCurrentContext() } |]


-- | Wraps @ImPlot::SetCurrentPlotContext()@.
setCurrentPlotContext :: MonadIO m => PlotContext -> m ()
setCurrentPlotContext (PlotContext contextPtr) = liftIO do
  [C.exp| void { SetCurrentContext($(ImPlotContext* contextPtr)) } |]

-- | Create demo window. Demonstrate most ImGui features. Call this to learn
-- about the library! Try to make it always available in your application!
showPlotDemoWindow :: (MonadIO m) => m ()
showPlotDemoWindow = liftIO do
  [C.exp| void { ShowDemoWindow(); } |]