{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibArtist class represents an artist, such as the performer of a song.
--
-- Generated bindings for @ITLibArtist@.
module ObjC.ITunesLibrary.ITLibArtist
  ( ITLibArtist
  , IsITLibArtist(..)
  , name
  , sortName
  , persistentID
  , nameSelector
  , persistentIDSelector
  , sortNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of this artist.
--
-- ObjC selector: @- name@
name :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSString)
name itLibArtist =
  sendMessage itLibArtist nameSelector

-- | The name of this artist that should be used for sorting purposes.
--
-- ObjC selector: @- sortName@
sortName :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSString)
sortName itLibArtist =
  sendMessage itLibArtist sortNameSelector

-- | The unique identifier of this artist.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibArtist itLibArtist => itLibArtist -> IO (Id NSNumber)
persistentID itLibArtist =
  sendMessage itLibArtist persistentIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @sortName@
sortNameSelector :: Selector '[] (Id NSString)
sortNameSelector = mkSelector "sortName"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] (Id NSNumber)
persistentIDSelector = mkSelector "persistentID"

