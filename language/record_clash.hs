{- 
Reference: https://gitlab.haskell.org/ghc/ghc/wikis/records

The narrow issue: namespacing for record field names. Currently in Haskell 
two records in the same module can't share a field name.  This is sometimes 
extremely painful.  This page is about the narrow issue.

The broad issue: first class record types.  In Haskell there is no "record 
type" per se. Rather, you can simply give names to the fields of a constructor.  
Records are not extensible and there is no polymorphism on records.

Compiling this file results in:
record.hs:2:34:
    Multiple declarations of `Main.a'
    Declared at: record.hs:1:24
                 record.hs:2:34
-}

data Record = Record { a :: String }
data RecordClash = RecordClash { a :: String }

main = print "Hello"