{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNTimingFunction@.
module ObjC.SceneKit.SCNTimingFunction
  ( SCNTimingFunction
  , IsSCNTimingFunction(..)
  , functionWithTimingMode
  , functionWithCAMediaTimingFunction
  , functionWithTimingModeSelector
  , functionWithCAMediaTimingFunctionSelector

  -- * Enum types
  , SCNActionTimingMode(SCNActionTimingMode)
  , pattern SCNActionTimingModeLinear
  , pattern SCNActionTimingModeEaseIn
  , pattern SCNActionTimingModeEaseOut
  , pattern SCNActionTimingModeEaseInEaseOut

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "functionWithTimingMode:") (retPtr retVoid) [argCLong (coerce timingMode)] >>= retainedObject . castPtr

-- | @+ functionWithCAMediaTimingFunction:@
functionWithCAMediaTimingFunction :: IsCAMediaTimingFunction caTimingFunction => caTimingFunction -> IO (Id SCNTimingFunction)
functionWithCAMediaTimingFunction caTimingFunction =
  do
    cls' <- getRequiredClass "SCNTimingFunction"
    withObjCPtr caTimingFunction $ \raw_caTimingFunction ->
      sendClassMsg cls' (mkSelector "functionWithCAMediaTimingFunction:") (retPtr retVoid) [argPtr (castPtr raw_caTimingFunction :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithTimingMode:@
functionWithTimingModeSelector :: Selector
functionWithTimingModeSelector = mkSelector "functionWithTimingMode:"

-- | @Selector@ for @functionWithCAMediaTimingFunction:@
functionWithCAMediaTimingFunctionSelector :: Selector
functionWithCAMediaTimingFunctionSelector = mkSelector "functionWithCAMediaTimingFunction:"

