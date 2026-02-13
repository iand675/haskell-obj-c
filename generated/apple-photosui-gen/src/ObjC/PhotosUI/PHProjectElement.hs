{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PHProjectElement is the superclass for all element objects. It is never directly used, but defines the shared properties of any element in an instance of PHProjectSectionContent.
--
-- Generated bindings for @PHProjectElement@.
module ObjC.PhotosUI.PHProjectElement
  ( PHProjectElement
  , IsPHProjectElement(..)
  , init_
  , new
  , weight
  , initSelector
  , newSelector
  , weightSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHProjectElement phProjectElement => phProjectElement -> IO (Id PHProjectElement)
init_ phProjectElement =
  sendOwnedMessage phProjectElement initSelector

-- | @+ new@
new :: IO (Id PHProjectElement)
new  =
  do
    cls' <- getRequiredClass "PHProjectElement"
    sendOwnedClassMessage cls' newSelector

-- | Relative significance of any element in the section content is defined by it's weight. Values range from 0.0 to 1.0 where the higher numbers represent higher overall significance. Projects that allow a user to reduce the number of elements in any section content can use this hint to determine which elements are most important to keep in order to preserve context. Default is 0.5.
--
-- ObjC selector: @- weight@
weight :: IsPHProjectElement phProjectElement => phProjectElement -> IO CDouble
weight phProjectElement =
  sendMessage phProjectElement weightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHProjectElement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHProjectElement)
newSelector = mkSelector "new"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CDouble
weightSelector = mkSelector "weight"

