import gleam/list
import gleeunit
import gleeunit/should
import odysseus

pub fn main() -> Nil {
  gleeunit.main()
}

const basic_conversions = [
  #("&gt;", ">"),
  #("&lt;", "<"),
  #("&quot;", "\""),
  #("&#39;", "'"),
  #("&amp;", "&"),
]

const named_entities = [
  #("&copy;", "©"),
  #("&reg;", "®"),
  #("&euro;", "€"),
  #("&pound;", "£"),
  #("&cent;", "¢"),
  #("&deg;", "°"),
  #("&bull;", "•"),
  #("&middot;", "·"),
  #("&ndash;", "–"),
  #("&mdash;", "—"),
  #("&lsquo;", "'"),
  #("&rsquo;", "'"),
  #("&ldquo;", "\""),
  #("&rdquo;", "\""),
  #("&hellip;", "…"),
  #("&trade;", "™"),
  #("&nbsp;", " "),
]

const numeric_entities = [
  #("&#8364;", "€"),
  // Decimal euro
  #("&#x20AC;", "€"),
  // Hex euro
  #("&#169;", "©"),
  // Decimal copyright
  #("&#xA9;", "©"),
  // Hex copyright
  #("&#8212;", "—"),
  // Decimal mdash
  #("&#x2014;", "—"),
  // Hex mdash
]

pub fn basic_conversions_test() {
  use #(escaped, value) <- list.each(basic_conversions)
  odysseus.unescape(escaped) |> should.equal(value)
}

pub fn named_entities_test() {
  use #(escaped, value) <- list.each(named_entities)
  odysseus.unescape(escaped) |> should.equal(value)
}

pub fn numeric_entities_test() {
  use #(escaped, value) <- list.each(numeric_entities)
  odysseus.unescape(escaped) |> should.equal(value)
}

pub fn mixed_entities_test() {
  let input =
    "&lt;div class=&quot;container&quot;&gt;Copy&copy; 2023&ndash;2025&lt;/div&gt;"
  let expected = "<div class=\"container\">Copy© 2023–2025</div>"
  odysseus.unescape(input) |> should.equal(expected)
}

pub fn invalid_entities_are_preserved_test() {
  let cases = [
    #("&invalid;", "&invalid;"),
    #("&gt", "&gt"),
    #("&#", "&#"),
    #("&#x", "&#x"),
    #("&#123abc;", "&#123abc;"),
    #("&#xZZZ;", "&#xZZZ;"),
  ]

  use #(input, expected) <- list.each(cases)
  odysseus.unescape(input) |> should.equal(expected)
}

pub fn nested_entities_test() {
  let input = "&amp;gt;"
  odysseus.unescape(input) |> should.equal("&gt;")
}

pub fn concatenated_entities_test() {
  let input = "&copy;&reg;&trade;"
  odysseus.unescape(input) |> should.equal("©®™")
}
