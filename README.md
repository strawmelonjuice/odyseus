# odyseus

[![Package Version](https://img.shields.io/hexpm/v/odyseus)](https://hex.pm/packages/odyseus)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/odyseus/)

[Houdini](https://hexdocs.pm/houdini/) was a master of escaping, Odyseus bound himself to his ship to
survive the sirens... thereby being a master of _unescaping_, I guess.

```sh
gleam add odyseus@1
```

```gleam
import odyseus
import houdini

pub fn main() -> Nil {
  // Escaped String
  let escaped_string = houdini.escape("<p>My <i>milkshake</i> brings all the <b>boys</b> to the yard...</p>")
  // Escaped Sting
  let escaped_sting = houdini.escape("I don't drink coffee, I'll take <u>tea</u> my dear<br>I like my <span style='background-color: black; color: white'>toast</span> done on one side<br>And you can hear it in my <i>accent</i> when I talk<br>I'm an <b>Englishman</b> in New York")


  // Now, time to unescape them to get the originals!
  let unescaped_string = odyseus.unescape(escaped_string)
  let caught_sting = odyseus.unescape(escaped_sting)
}
```

Further documentation can be found at <https://hexdocs.pm/odyseus>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
