{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @TKTokenWatcher@.
module ObjC.CryptoTokenKit.TKTokenWatcher
  ( TKTokenWatcher
  , IsTKTokenWatcher(..)
  , init_
  , initWithInsertionHandler
  , setInsertionHandler
  , addRemovalHandler_forTokenID
  , tokenInfoForTokenID
  , tokenIDs
  , addRemovalHandler_forTokenIDSelector
  , initSelector
  , initWithInsertionHandlerSelector
  , setInsertionHandlerSelector
  , tokenIDsSelector
  , tokenInfoForTokenIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init watcher
--
-- ObjC selector: @- init@
init_ :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> IO (Id TKTokenWatcher)
init_ tkTokenWatcher =
  sendOwnedMessage tkTokenWatcher initSelector

-- | Init watcher with insertion handler
--
-- init watcher with insertion handler which is called when a new token arrives
--
-- @insertionHandler@ — called when a new token is inserted
--
-- ObjC selector: @- initWithInsertionHandler:@
initWithInsertionHandler :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> Ptr () -> IO (Id TKTokenWatcher)
initWithInsertionHandler tkTokenWatcher insertionHandler =
  sendOwnedMessage tkTokenWatcher initWithInsertionHandlerSelector insertionHandler

-- | Set insertion handler
--
-- when an insertion handler is set the TokenWatcher will call this handler when new token appears in the system. TokenWatcher will call the handler also for tokens which was registered in the system before the handler was set.
--
-- @insertionHandler@ — called when a new token is inserted
--
-- ObjC selector: @- setInsertionHandler:@
setInsertionHandler :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> Ptr () -> IO ()
setInsertionHandler tkTokenWatcher insertionHandler =
  sendMessage tkTokenWatcher setInsertionHandlerSelector insertionHandler

-- | Add removal watcher for specific tokenID
--
-- after removalHandler for a specific tokenID is called the reference to this handler is removed. For one tokenID just one handler can be added, so next call to addRemovalHandler will replace previous handler
--
-- @removalHandler@ — called when a token is removed
--
-- @tokenID@ — specified tokenID, if tokenID does not exist removal handler is called imediately
--
-- ObjC selector: @- addRemovalHandler:forTokenID:@
addRemovalHandler_forTokenID :: (IsTKTokenWatcher tkTokenWatcher, IsNSString tokenID) => tkTokenWatcher -> Ptr () -> tokenID -> IO ()
addRemovalHandler_forTokenID tkTokenWatcher removalHandler tokenID =
  sendMessage tkTokenWatcher addRemovalHandler_forTokenIDSelector removalHandler (toNSString tokenID)

-- | Return TokenInfo for specific tokenID
--
-- @tokenID@ — specified tokenID
--
-- Returns: A TokenInfo object, or nil if tokenID does not exist
--
-- ObjC selector: @- tokenInfoForTokenID:@
tokenInfoForTokenID :: (IsTKTokenWatcher tkTokenWatcher, IsNSString tokenID) => tkTokenWatcher -> tokenID -> IO (Id TKTokenWatcherTokenInfo)
tokenInfoForTokenID tkTokenWatcher tokenID =
  sendMessage tkTokenWatcher tokenInfoForTokenIDSelector (toNSString tokenID)

-- | Array of currently known TokenIDs in the system.  Tokens are identified by instance's names. It is possible to use KVO to be notified about token arrivals and removals.
--
-- ObjC selector: @- tokenIDs@
tokenIDs :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> IO (Id NSArray)
tokenIDs tkTokenWatcher =
  sendMessage tkTokenWatcher tokenIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKTokenWatcher)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInsertionHandler:@
initWithInsertionHandlerSelector :: Selector '[Ptr ()] (Id TKTokenWatcher)
initWithInsertionHandlerSelector = mkSelector "initWithInsertionHandler:"

-- | @Selector@ for @setInsertionHandler:@
setInsertionHandlerSelector :: Selector '[Ptr ()] ()
setInsertionHandlerSelector = mkSelector "setInsertionHandler:"

-- | @Selector@ for @addRemovalHandler:forTokenID:@
addRemovalHandler_forTokenIDSelector :: Selector '[Ptr (), Id NSString] ()
addRemovalHandler_forTokenIDSelector = mkSelector "addRemovalHandler:forTokenID:"

-- | @Selector@ for @tokenInfoForTokenID:@
tokenInfoForTokenIDSelector :: Selector '[Id NSString] (Id TKTokenWatcherTokenInfo)
tokenInfoForTokenIDSelector = mkSelector "tokenInfoForTokenID:"

-- | @Selector@ for @tokenIDs@
tokenIDsSelector :: Selector '[] (Id NSArray)
tokenIDsSelector = mkSelector "tokenIDs"

