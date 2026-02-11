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
  , initSelector
  , fieldsSelector
  , setFieldsSelector
  , fieldsIncludedAreClearedSelector
  , setFieldsIncludedAreClearedSelector


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

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO (Id PDFActionResetForm)
init_ pdfActionResetForm  =
  sendMsg pdfActionResetForm (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- fields@
fields :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO (Id NSArray)
fields pdfActionResetForm  =
  sendMsg pdfActionResetForm (mkSelector "fields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFields:@
setFields :: (IsPDFActionResetForm pdfActionResetForm, IsNSArray value) => pdfActionResetForm -> value -> IO ()
setFields pdfActionResetForm  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfActionResetForm (mkSelector "setFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fieldsIncludedAreCleared@
fieldsIncludedAreCleared :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> IO Bool
fieldsIncludedAreCleared pdfActionResetForm  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfActionResetForm (mkSelector "fieldsIncludedAreCleared") retCULong []

-- | @- setFieldsIncludedAreCleared:@
setFieldsIncludedAreCleared :: IsPDFActionResetForm pdfActionResetForm => pdfActionResetForm -> Bool -> IO ()
setFieldsIncludedAreCleared pdfActionResetForm  value =
  sendMsg pdfActionResetForm (mkSelector "setFieldsIncludedAreCleared:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @fields@
fieldsSelector :: Selector
fieldsSelector = mkSelector "fields"

-- | @Selector@ for @setFields:@
setFieldsSelector :: Selector
setFieldsSelector = mkSelector "setFields:"

-- | @Selector@ for @fieldsIncludedAreCleared@
fieldsIncludedAreClearedSelector :: Selector
fieldsIncludedAreClearedSelector = mkSelector "fieldsIncludedAreCleared"

-- | @Selector@ for @setFieldsIncludedAreCleared:@
setFieldsIncludedAreClearedSelector :: Selector
setFieldsIncludedAreClearedSelector = mkSelector "setFieldsIncludedAreCleared:"

