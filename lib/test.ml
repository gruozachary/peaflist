open! Base

let symbols =
  match
    Trie.of_alist Trie.Of_string.Keychain.keychainable [ ("-", ()); ("->", ()) ]
  with
  | Ok s -> s
  | _ -> assert false
