{-# LANGUAGE DataKinds #-}
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
  , identifier
  , url
  , identifierSelector
  , initSelector
  , newSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYou.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWHighlight swHighlight => swHighlight -> IO (Id SWHighlight)
init_ swHighlight =
  sendOwnedMessage swHighlight initSelector

-- | @+ new@
new :: IO (Id SWHighlight)
new  =
  do
    cls' <- getRequiredClass "SWHighlight"
    sendOwnedClassMessage cls' newSelector

-- | The unique identifier for this highlight
--
-- ObjC selector: @- identifier@
identifier :: IsSWHighlight swHighlight => swHighlight -> IO RawId
identifier swHighlight =
  sendMessage swHighlight identifierSelector

-- | The surfaced content URL
--
-- ObjC selector: @- URL@
url :: IsSWHighlight swHighlight => swHighlight -> IO (Id NSURL)
url swHighlight =
  sendMessage swHighlight urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWHighlight)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWHighlight)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] RawId
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

