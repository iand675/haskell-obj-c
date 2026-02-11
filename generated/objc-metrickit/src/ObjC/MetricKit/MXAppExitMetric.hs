{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXAppExitMetric
--
-- A class that encapsulates application exit metrics for both on screen and off screen exits.
--
-- Application exits can be expected, such as when the application is killed in the app switcher by the user, or unexpected, such as when a runtime error occurs.
--
-- Minimizing unexpected exits and maximizing expected exits can improve performance and reliability of your application.
--
-- Generated bindings for @MXAppExitMetric@.
module ObjC.MetricKit.MXAppExitMetric
  ( MXAppExitMetric
  , IsMXAppExitMetric(..)
  , foregroundExitData
  , backgroundExitData
  , foregroundExitDataSelector
  , backgroundExitDataSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | foregroundExitData
--
-- Cumulative foreground exit data.
--
-- This includes application exit data when the application was on screen and visible to the user.
--
-- ObjC selector: @- foregroundExitData@
foregroundExitData :: IsMXAppExitMetric mxAppExitMetric => mxAppExitMetric -> IO (Id MXForegroundExitData)
foregroundExitData mxAppExitMetric  =
  sendMsg mxAppExitMetric (mkSelector "foregroundExitData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | backgroundExitData
--
-- Cumulative background exit data.
--
-- This includes application exit data when the application was off screen and not visible to the user.
--
-- ObjC selector: @- backgroundExitData@
backgroundExitData :: IsMXAppExitMetric mxAppExitMetric => mxAppExitMetric -> IO (Id MXBackgroundExitData)
backgroundExitData mxAppExitMetric  =
  sendMsg mxAppExitMetric (mkSelector "backgroundExitData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @foregroundExitData@
foregroundExitDataSelector :: Selector
foregroundExitDataSelector = mkSelector "foregroundExitData"

-- | @Selector@ for @backgroundExitData@
backgroundExitDataSelector :: Selector
backgroundExitDataSelector = mkSelector "backgroundExitData"

