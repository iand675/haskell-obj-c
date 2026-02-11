{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWHighlight
--
-- A model object representing a universal link shared by any number of contacts, in any number of conversations. The identities of the contacts are not exposed to the application.
--
-- The system tracks universal links shared with the current user, and decides which links to elevate for consumption in an app. When the system deems a link to be useful, it surfaces that link to the hosting app in the form of an @SWHighlight@ object. Only universal links can be surfaced in this way.
--
-- Generated bindings for @SWHighlight@.
module ObjC.SharedWithYou.SWHighlight
  ( SWHighlight
  , IsSWHighlight(..)
  , init_
  , new
  , url
  , initSelector
  , newSelector
  , urlSelector


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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWHighlight swHighlight => swHighlight -> IO (Id SWHighlight)
init_ swHighlight  =
  sendMsg swHighlight (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWHighlight)
new  =
  do
    cls' <- getRequiredClass "SWHighlight"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The surfaced content URL
--
-- ObjC selector: @- URL@
url :: IsSWHighlight swHighlight => swHighlight -> IO (Id NSURL)
url swHighlight  =
  sendMsg swHighlight (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

