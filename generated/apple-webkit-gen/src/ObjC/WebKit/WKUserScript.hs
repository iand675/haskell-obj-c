{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , forMainFrameOnlySelector
  , initWithSource_injectionTime_forMainFrameOnlySelector
  , initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector
  , injectionTimeSelector
  , sourceSelector

  -- * Enum types
  , WKUserScriptInjectionTime(WKUserScriptInjectionTime)
  , pattern WKUserScriptInjectionTimeAtDocumentStart
  , pattern WKUserScriptInjectionTimeAtDocumentEnd

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithSource_injectionTime_forMainFrameOnly wkUserScript source injectionTime forMainFrameOnly =
  sendOwnedMessage wkUserScript initWithSource_injectionTime_forMainFrameOnlySelector (toNSString source) injectionTime forMainFrameOnly

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
initWithSource_injectionTime_forMainFrameOnly_inContentWorld wkUserScript source injectionTime forMainFrameOnly contentWorld =
  sendOwnedMessage wkUserScript initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector (toNSString source) injectionTime forMainFrameOnly (toWKContentWorld contentWorld)

-- | @- source@
source :: IsWKUserScript wkUserScript => wkUserScript -> IO (Id NSString)
source wkUserScript =
  sendMessage wkUserScript sourceSelector

-- | @- injectionTime@
injectionTime :: IsWKUserScript wkUserScript => wkUserScript -> IO WKUserScriptInjectionTime
injectionTime wkUserScript =
  sendMessage wkUserScript injectionTimeSelector

-- | @- forMainFrameOnly@
forMainFrameOnly :: IsWKUserScript wkUserScript => wkUserScript -> IO Bool
forMainFrameOnly wkUserScript =
  sendMessage wkUserScript forMainFrameOnlySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:injectionTime:forMainFrameOnly:@
initWithSource_injectionTime_forMainFrameOnlySelector :: Selector '[Id NSString, WKUserScriptInjectionTime, Bool] (Id WKUserScript)
initWithSource_injectionTime_forMainFrameOnlySelector = mkSelector "initWithSource:injectionTime:forMainFrameOnly:"

-- | @Selector@ for @initWithSource:injectionTime:forMainFrameOnly:inContentWorld:@
initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector :: Selector '[Id NSString, WKUserScriptInjectionTime, Bool, Id WKContentWorld] (Id WKUserScript)
initWithSource_injectionTime_forMainFrameOnly_inContentWorldSelector = mkSelector "initWithSource:injectionTime:forMainFrameOnly:inContentWorld:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @injectionTime@
injectionTimeSelector :: Selector '[] WKUserScriptInjectionTime
injectionTimeSelector = mkSelector "injectionTime"

-- | @Selector@ for @forMainFrameOnly@
forMainFrameOnlySelector :: Selector '[] Bool
forMainFrameOnlySelector = mkSelector "forMainFrameOnly"

