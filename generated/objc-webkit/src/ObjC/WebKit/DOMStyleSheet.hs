{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMStyleSheet@.
module ObjC.WebKit.DOMStyleSheet
  ( DOMStyleSheet
  , IsDOMStyleSheet(..)
  , type_
  , disabled
  , setDisabled
  , ownerNode
  , parentStyleSheet
  , href
  , title
  , media
  , typeSelector
  , disabledSelector
  , setDisabledSelector
  , ownerNodeSelector
  , parentStyleSheetSelector
  , hrefSelector
  , titleSelector
  , mediaSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
type_ domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- disabled@
disabled :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO Bool
disabled domStyleSheet  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domStyleSheet (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> Bool -> IO ()
setDisabled domStyleSheet  value =
  sendMsg domStyleSheet (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- ownerNode@
ownerNode :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMNode)
ownerNode domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "ownerNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parentStyleSheet@
parentStyleSheet :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMStyleSheet)
parentStyleSheet domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "parentStyleSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- href@
href :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
href domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
title domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- media@
media :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMMediaList)
media domStyleSheet  =
  sendMsg domStyleSheet (mkSelector "media") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @ownerNode@
ownerNodeSelector :: Selector
ownerNodeSelector = mkSelector "ownerNode"

-- | @Selector@ for @parentStyleSheet@
parentStyleSheetSelector :: Selector
parentStyleSheetSelector = mkSelector "parentStyleSheet"

-- | @Selector@ for @href@
hrefSelector :: Selector
hrefSelector = mkSelector "href"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @media@
mediaSelector :: Selector
mediaSelector = mkSelector "media"

