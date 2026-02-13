{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFActionResetForm@.
module ObjC.PDFKit.PDFActionResetForm
  ( PDFActionResetForm
  , IsPDFActionResetForm(..)
  , init_
  , fields
  , setFields
  , fieldsIncludedAreCleared
  , setFieldsIncludedAreCleared
  , fieldsIncludedAreClearedSelector
  , fieldsSelector
  , initSelector
  , setFieldsIncludedAreClearedSelector
  , setFieldsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO (Id PDFActionResetForm)
init_ pdfActionResetForm =
  sendOwnedMessage pdfActionResetForm initSelector

-- | @- fields@
fields :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO (Id NSArray)
fields pdfActionResetForm =
  sendMessage pdfActionResetForm fieldsSelector

-- | @- setFields:@
setFields :: (IsPDFActionResetForm pdfActionResetForm, IsNSArray value) => pdfActionResetForm -> value -> IO ()
setFields pdfActionResetForm value =
  sendMessage pdfActionResetForm setFieldsSelector (toNSArray value)

-- | @- fieldsIncludedAreCleared@
fieldsIncludedAreCleared :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO Bool
fieldsIncludedAreCleared pdfActionResetForm =
  sendMessage pdfActionResetForm fieldsIncludedAreClearedSelector

-- | @- setFieldsIncludedAreCleared:@
setFieldsIncludedAreCleared :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> Bool -> IO ()
setFieldsIncludedAreCleared pdfActionResetForm value =
  sendMessage pdfActionResetForm setFieldsIncludedAreClearedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PDFActionResetForm)
initSelector = mkSelector "init"

-- | @Selector@ for @fields@
fieldsSelector :: Selector '[] (Id NSArray)
fieldsSelector = mkSelector "fields"

-- | @Selector@ for @setFields:@
setFieldsSelector :: Selector '[Id NSArray] ()
setFieldsSelector = mkSelector "setFields:"

-- | @Selector@ for @fieldsIncludedAreCleared@
fieldsIncludedAreClearedSelector :: Selector '[] Bool
fieldsIncludedAreClearedSelector = mkSelector "fieldsIncludedAreCleared"

-- | @Selector@ for @setFieldsIncludedAreCleared:@
setFieldsIncludedAreClearedSelector :: Selector '[Bool] ()
setFieldsIncludedAreClearedSelector = mkSelector "setFieldsIncludedAreCleared:"

