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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHProjectElement phProjectElement => phProjectElement -> IO (Id PHProjectElement)
init_ phProjectElement  =
  sendMsg phProjectElement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHProjectElement)
new  =
  do
    cls' <- getRequiredClass "PHProjectElement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Relative significance of any element in the section content is defined by it's weight. Values range from 0.0 to 1.0 where the higher numbers represent higher overall significance. Projects that allow a user to reduce the number of elements in any section content can use this hint to determine which elements are most important to keep in order to preserve context. Default is 0.5.
--
-- ObjC selector: @- weight@
weight :: IsPHProjectElement phProjectElement => phProjectElement -> IO CDouble
weight phProjectElement  =
  sendMsg phProjectElement (mkSelector "weight") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

