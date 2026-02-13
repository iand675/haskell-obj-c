{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of this class can be used to change the visual style of recipeint email address token when user in composing a mail message.
--
-- Generated bindings for @MEAddressAnnotation@.
module ObjC.MailKit.MEAddressAnnotation
  ( MEAddressAnnotation
  , IsMEAddressAnnotation(..)
  , init_
  , new
  , errorWithLocalizedDescription
  , warningWithLocalizedDescription
  , successWithLocalizedDescription
  , errorWithLocalizedDescriptionSelector
  , initSelector
  , newSelector
  , successWithLocalizedDescriptionSelector
  , warningWithLocalizedDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMEAddressAnnotation meAddressAnnotation => meAddressAnnotation -> IO (Id MEAddressAnnotation)
init_ meAddressAnnotation =
  sendOwnedMessage meAddressAnnotation initSelector

-- | @+ new@
new :: IO (Id MEAddressAnnotation)
new  =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    sendOwnedClassMessage cls' newSelector

-- | An annotation to denote a recipeint email address has an error when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description of the error that may be presented to the user.
--
-- ObjC selector: @+ errorWithLocalizedDescription:@
errorWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
errorWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    sendClassMessage cls' errorWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- | An annotation to warn about a recipeint email address when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description of the warning may be presented to the user. .
--
-- ObjC selector: @+ warningWithLocalizedDescription:@
warningWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
warningWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    sendClassMessage cls' warningWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- | An annotation to  denote a valid recipeint email address when composing a mail message.
--
-- @localizedDescription@ — - A localized string with a brief description that may be presented to the user. .
--
-- ObjC selector: @+ successWithLocalizedDescription:@
successWithLocalizedDescription :: IsNSString localizedDescription => localizedDescription -> IO (Id MEAddressAnnotation)
successWithLocalizedDescription localizedDescription =
  do
    cls' <- getRequiredClass "MEAddressAnnotation"
    sendClassMessage cls' successWithLocalizedDescriptionSelector (toNSString localizedDescription)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEAddressAnnotation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEAddressAnnotation)
newSelector = mkSelector "new"

-- | @Selector@ for @errorWithLocalizedDescription:@
errorWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id MEAddressAnnotation)
errorWithLocalizedDescriptionSelector = mkSelector "errorWithLocalizedDescription:"

-- | @Selector@ for @warningWithLocalizedDescription:@
warningWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id MEAddressAnnotation)
warningWithLocalizedDescriptionSelector = mkSelector "warningWithLocalizedDescription:"

-- | @Selector@ for @successWithLocalizedDescription:@
successWithLocalizedDescriptionSelector :: Selector '[Id NSString] (Id MEAddressAnnotation)
successWithLocalizedDescriptionSelector = mkSelector "successWithLocalizedDescription:"

