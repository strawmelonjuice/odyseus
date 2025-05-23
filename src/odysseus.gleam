/// Unescapes a string by converting HTML entities back to their original characters:
///   - `&lt;` becomes `<`
///   - `&gt;` becomes `>`
///   - `&amp;` becomes `&`
///   - `&quot;` becomes `"`
///   - `&#39;` becomes `'`
/// etc.
@external(erlang, "odysseus_ffi", "do_unescape")
@external(javascript, "./odysseus_ffi.js", "do_unescape")
pub fn unescape(escaped: String) -> String
