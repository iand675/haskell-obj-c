{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A
--
-- WKUserScript
--
-- object represents a script that can be injected into webpages.
--
-- Generated bindings for @WKUserScript@.
module ObjC.WebKit.WKUserScript
  ( WKUserScript
  , IsWKUserScript(..)
  , initWithSource_injectionTime_forMainFrameOnly
  , initWithSource_injectionTime_forMainFrameOnly_inContentWorld
  , source
  , injectionTime
  , forMainFrameOnly
  , initWithSource_injectionTime_forMainFrameOnlySelector
  , initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector
  , sourceSelector
  , injectionTimeSelector
  , forMainFrameOnlySelector

  -- * Enum types
  , WKUserScriptInjectionTime(WKUserScriptInjectionTime)
  , pattern WKUserScriptInjectionTimeAtDocumentStart
  , pattern WKUserScriptInjectionTimeAtDocumentEnd

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

import ObjC.WebKit.Internal.Classes
import ObjC.WebKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns an initialized user script that can be added to a
--
-- WKUserContentController
--
-- .
--
-- @source@ — The script source.
--
-- @injectionTime@ — When the script should be injected.
--
-- @forMainFrameOnly@ — Whether the script should be injected into all frames or just the main frame.
--
-- Calling this method is the same as calling @initWithSource:injectionTime:forMainFrameOnly:inContentWorld:@ with a @contentWorld@ value of @WKContentWorld.pageWorld@
--
-- ObjC selector: @- initWithSource:injectionTime:forMainFrameOnly:@
initWithSource_injectionTime_forMainFrameOnly :: (IsWKUserScript wkUserScript, IsNSString source) => wkUserScript -> source -> WKUserScriptInjectionTime -> Bool -> IO (Id WKUserScript)
initWithSource_injectionTime_forMainFrameOnly wkUserScript  source injectionTime forMainFrameOnly =
withObjCPtr source $ \raw_source ->
    sendMsg wkUserScript (mkSelector "initWithSource:injectionTime:forMainFrameOnly:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCLong (coerce injectionTime), argCULong (if forMainFrameOnly then 1 else 0)] >>= ownedObject . castPtr

-- | Returns an initialized user script that can be added to a
--
-- WKUserContentController
--
-- .
--
-- @source@ — The script source.
--
-- @injectionTime@ — When the script should be injected.
--
-- @forMainFrameOnly@ — Whether the script should be injected into all frames or just the main frame.
--
-- @contentWorld@ — The WKContentWorld in which to inject the script.
--
-- ObjC selector: @- initWithSource:injectionTime:forMainFrameOnly:inContentWorld:@
initWithSource_injectionTime_forMainFrameOnly_inContentWorld :: (IsWKUserScript wkUserScript, IsNSString source, IsWKContentWorld contentWorld) => wkUserScript -> source -> WKUserScriptInjectionTime -> Bool -> contentWorld -> IO (Id WKUserScript)
initWithSource_injectionTime_forMainFrameOnly_inContentWorld wkUserScript  source injectionTime forMainFrameOnly contentWorld =
withObjCPtr source $ \raw_source ->
  withObjCPtr contentWorld $ \raw_contentWorld ->
      sendMsg wkUserScript (mkSelector "initWithSource:injectionTime:forMainFrameOnly:inContentWorld:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argCLong (coerce injectionTime), argCULong (if forMainFrameOnly then 1 else 0), argPtr (castPtr raw_contentWorld :: Ptr ())] >>= ownedObject . castPtr

-- | @- source@
source :: IsWKUserScript wkUserScript => wkUserScript -> IO (Id NSString)
source wkUserScript  =
  sendMsg wkUserScript (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- injectionTime@
injectionTime :: IsWKUserScript wkUserScript => wkUserScript -> IO WKUserScriptInjectionTime
injectionTime wkUserScript  =
  fmap (coerce :: CLong -> WKUserScriptInjectionTime) $ sendMsg wkUserScript (mkSelector "injectionTime") retCLong []

-- | @- forMainFrameOnly@
forMainFrameOnly :: IsWKUserScript wkUserScript => wkUserScript -> IO Bool
forMainFrameOnly wkUserScript  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkUserScript (mkSelector "forMainFrameOnly") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:injectionTime:forMainFrameOnly:@
initWithSource_injectionTime_forMainFrameOnlySelector :: Selector
initWithSource_injectionTime_forMainFrameOnlySelector = mkSelector "initWithSource:injectionTime:forMainFrameOnly:"

-- | @Selector@ for @initWithSource:injectionTime:forMainFrameOnly:inContentWorld:@
initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector :: Selector
initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector = mkSelector "initWithSource:injectionTime:forMainFrameOnly:inContentWorld:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @injectionTime@
injectionTimeSelector :: Selector
injectionTimeSelector = mkSelector "injectionTime"

-- | @Selector@ for @forMainFrameOnly@
forMainFrameOnlySelector :: Selector
forMainFrameOnlySelector = mkSelector "forMainFrameOnly"

