# reflex-dom-attrs
Reflex-DOM extended attributes

```
elDiv [ "class" ~: "some static class"
      , "class" ~: someDynClass
      , "style" ~: ["display" ~:: "flex", "opacity" ~:: "1"]
      , "style" ~: someDynStyle
      , "some_attribute" ~: "attribute_value"
      , "another_attribute" ~: someDynAttrValue
      , _self_ $ \(e :: Element ev d t) -> do
          someDynAttrsThatDependOnSelfElement <- foldDyn someFunc initAttrs e
          pure someDynAttrsThatDependOnSelfElement
      ]
```
