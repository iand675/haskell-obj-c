{-# LANGUAGE DataKinds #-}
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
  , disabledSelector
  , hrefSelector
  , mediaSelector
  , ownerNodeSelector
  , parentStyleSheetSelector
  , setDisabledSelector
  , titleSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
type_ domStyleSheet =
  sendMessage domStyleSheet typeSelector

-- | @- disabled@
disabled :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO Bool
disabled domStyleSheet =
  sendMessage domStyleSheet disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> Bool -> IO ()
setDisabled domStyleSheet value =
  sendMessage domStyleSheet setDisabledSelector value

-- | @- ownerNode@
ownerNode :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMNode)
ownerNode domStyleSheet =
  sendMessage domStyleSheet ownerNodeSelector

-- | @- parentStyleSheet@
parentStyleSheet :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMStyleSheet)
parentStyleSheet domStyleSheet =
  sendMessage domStyleSheet parentStyleSheetSelector

-- | @- href@
href :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
href domStyleSheet =
  sendMessage domStyleSheet hrefSelector

-- | @- title@
title :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id NSString)
title domStyleSheet =
  sendMessage domStyleSheet titleSelector

-- | @- media@
media :: IsDOMStyleSheet domStyleSheet => domStyleSheet -> IO (Id DOMMediaList)
media domStyleSheet =
  sendMessage domStyleSheet mediaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @ownerNode@
ownerNodeSelector :: Selector '[] (Id DOMNode)
ownerNodeSelector = mkSelector "ownerNode"

-- | @Selector@ for @parentStyleSheet@
parentStyleSheetSelector :: Selector '[] (Id DOMStyleSheet)
parentStyleSheetSelector = mkSelector "parentStyleSheet"

-- | @Selector@ for @href@
hrefSelector :: Selector '[] (Id NSString)
hrefSelector = mkSelector "href"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @media@
mediaSelector :: Selector '[] (Id DOMMediaList)
mediaSelector = mkSelector "media"

