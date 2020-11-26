# hocon

Small library for [Typesafe's configuration specification](https://github.com/lightbend/config) built with parsec.

```
foo = bar
bar = 123
someObject {
    wow = "this is nice!"
}
```

While there are many configuration notations and formats around, HOCON stands in a comfortable place, albeit a bit underrated. Being a superset to JSON means it inherits its great readability, as well as extending it with some very nice features, such as comments, no need to add `"` for properties and values (unless some special character is required), digging up nested values, among others.

## Sounds nice, how do I use it?

First of all, import the basic stuff:
```haskell
import Data.HOCON
import Text.Parser.HOCON
```

The first module exports the tree-like structure typical of JSON (if you're familiar with `aeson`, then it's really the same structure. It's not using _that_ structure because no way I'm including that just for the structure) and the accessor functions, while the second exports the parser function:
```haskell
parseHOCON :: String -> Either ParseError Config
```

If you've already used Typesafe's config on the JVM, then the functions will make you feel just like at home:
```haskell
getConfig :: String -> Config -> Maybe Config
getNumber :: String -> Config -> Maybe Double
getString :: String -> Config -> Maybe String
getBoolean :: String -> Config -> Maybe Bool
getList :: String -> Config -> Maybe [Config]
hasPath :: String -> Config -> Bool
```

## Missing features / what's to come
* Actually support comments
* Multiline strings
* Include another file and merge them
* Object substitution

Pull requests are always welcome!
