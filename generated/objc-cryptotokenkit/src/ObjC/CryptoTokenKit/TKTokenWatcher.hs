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
  , initSelector
  , initWithInsertionHandlerSelector
  , setInsertionHandlerSelector
  , addRemovalHandler_forTokenIDSelector
  , tokenInfoForTokenIDSelector
  , tokenIDsSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Init watcher
--
-- ObjC selector: @- init@
init_ :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> IO (Id TKTokenWatcher)
init_ tkTokenWatcher  =
  sendMsg tkTokenWatcher (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Init watcher with insertion handler
--
-- init watcher with insertion handler which is called when a new token arrives
--
-- @insertionHandler@ — called when a new token is inserted
--
-- ObjC selector: @- initWithInsertionHandler:@
initWithInsertionHandler :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> Ptr () -> IO (Id TKTokenWatcher)
initWithInsertionHandler tkTokenWatcher  insertionHandler =
  sendMsg tkTokenWatcher (mkSelector "initWithInsertionHandler:") (retPtr retVoid) [argPtr (castPtr insertionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Set insertion handler
--
-- when an insertion handler is set the TokenWatcher will call this handler when new token appears in the system. TokenWatcher will call the handler also for tokens which was registered in the system before the handler was set.
--
-- @insertionHandler@ — called when a new token is inserted
--
-- ObjC selector: @- setInsertionHandler:@
setInsertionHandler :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> Ptr () -> IO ()
setInsertionHandler tkTokenWatcher  insertionHandler =
  sendMsg tkTokenWatcher (mkSelector "setInsertionHandler:") retVoid [argPtr (castPtr insertionHandler :: Ptr ())]

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
addRemovalHandler_forTokenID tkTokenWatcher  removalHandler tokenID =
withObjCPtr tokenID $ \raw_tokenID ->
    sendMsg tkTokenWatcher (mkSelector "addRemovalHandler:forTokenID:") retVoid [argPtr (castPtr removalHandler :: Ptr ()), argPtr (castPtr raw_tokenID :: Ptr ())]

-- | Return TokenInfo for specific tokenID
--
-- @tokenID@ — specified tokenID
--
-- Returns: A TokenInfo object, or nil if tokenID does not exist
--
-- ObjC selector: @- tokenInfoForTokenID:@
tokenInfoForTokenID :: (IsTKTokenWatcher tkTokenWatcher, IsNSString tokenID) => tkTokenWatcher -> tokenID -> IO (Id TKTokenWatcherTokenInfo)
tokenInfoForTokenID tkTokenWatcher  tokenID =
withObjCPtr tokenID $ \raw_tokenID ->
    sendMsg tkTokenWatcher (mkSelector "tokenInfoForTokenID:") (retPtr retVoid) [argPtr (castPtr raw_tokenID :: Ptr ())] >>= retainedObject . castPtr

-- | Array of currently known TokenIDs in the system.  Tokens are identified by instance's names. It is possible to use KVO to be notified about token arrivals and removals.
--
-- ObjC selector: @- tokenIDs@
tokenIDs :: IsTKTokenWatcher tkTokenWatcher => tkTokenWatcher -> IO (Id NSArray)
tokenIDs tkTokenWatcher  =
  sendMsg tkTokenWatcher (mkSelector "tokenIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInsertionHandler:@
initWithInsertionHandlerSelector :: Selector
initWithInsertionHandlerSelector = mkSelector "initWithInsertionHandler:"

-- | @Selector@ for @setInsertionHandler:@
setInsertionHandlerSelector :: Selector
setInsertionHandlerSelector = mkSelector "setInsertionHandler:"

-- | @Selector@ for @addRemovalHandler:forTokenID:@
addRemovalHandler_forTokenIDSelector :: Selector
addRemovalHandler_forTokenIDSelector = mkSelector "addRemovalHandler:forTokenID:"

-- | @Selector@ for @tokenInfoForTokenID:@
tokenInfoForTokenIDSelector :: Selector
tokenInfoForTokenIDSelector = mkSelector "tokenInfoForTokenID:"

-- | @Selector@ for @tokenIDs@
tokenIDsSelector :: Selector
tokenIDsSelector = mkSelector "tokenIDs"

