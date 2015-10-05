module Psc.Gui.Component.Util where

import Unsafe.Coerce

unsafeTargetValue :: forall a. a -> String
unsafeTargetValue ev = (unsafeCoerce ev).target.value
