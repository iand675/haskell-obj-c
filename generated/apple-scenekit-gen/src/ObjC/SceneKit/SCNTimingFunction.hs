{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNTimingFunction@.
module ObjC.SceneKit.SCNTimingFunction
  ( SCNTimingFunction
  , IsSCNTimingFunction(..)
  , functionWithTimingMode
  , functionWithCAMediaTimingFunction
  , functionWithCAMediaTimingFunctionSelector
  , functionWithTimingModeSelector

  -- * Enum types
  , SCNActionTimingMode(SCNActionTimingMode)
  , pattern SCNActionTimingModeLinear
  , pattern SCNActionTimingModeEaseIn
  , pattern SCNActionTimingModeEaseOut
  , pattern SCNActionTimingModeEaseInEaseOut

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @+ functionWithTimingMode:@
functionWithTimingMode :: SCNActionTimingMode -> IO (Id SCNTimingFunction)
functionWithTimingMode timingMode =
  do
    cls' <- getRequiredClass "SCNTimingFunction"
    sendClassMessage cls' functionWithTimingModeSelector timingMode

-- | @+ functionWithCAMediaTimingFunction:@
functionWithCAMediaTimingFunction :: IsCAMediaTimingFunction caTimingFunction => caTimingFunction -> IO (Id SCNTimingFunction)
functionWithCAMediaTimingFunction caTimingFunction =
  do
    cls' <- getRequiredClass "SCNTimingFunction"
    sendClassMessage cls' functionWithCAMediaTimingFunctionSelector (toCAMediaTimingFunction caTimingFunction)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithTimingMode:@
functionWithTimingModeSelector :: Selector '[SCNActionTimingMode] (Id SCNTimingFunction)
functionWithTimingModeSelector = mkSelector "functionWithTimingMode:"

-- | @Selector@ for @functionWithCAMediaTimingFunction:@
functionWithCAMediaTimingFunctionSelector :: Selector '[Id CAMediaTimingFunction] (Id SCNTimingFunction)
functionWithCAMediaTimingFunctionSelector = mkSelector "functionWithCAMediaTimingFunction:"

