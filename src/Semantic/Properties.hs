module Semantic.Properties (module Export) where

import Semantic.Properties.Active as Export
import Semantic.Properties.Animated as Export
import Semantic.Properties.As as Export
import Semantic.Properties.Attributes as Export
import Semantic.Properties.Avatar as Export
import Semantic.Properties.Basic as Export
import Semantic.Properties.Basic as Export
import Semantic.Properties.Children as Export
import Semantic.Properties.Classes as Export
import Semantic.Properties.Name as Export

infixl 1 !%
(!%) c cs as = Children (cs (Attributes as c))

infixl 1 %!
(%!) c as cs = Attributes (as (Children cs c))

pattern ToLeft = "left"
pattern ToRight = "right"
pattern ToTop = "top"
pattern ToBottom = "bottom"