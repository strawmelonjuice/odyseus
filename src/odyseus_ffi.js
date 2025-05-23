function do_unescape(escaped) {
  // Browser environment: Use native DOM parser
  if (typeof window !== "undefined") {
    const doc = new DOMParser().parseFromString(escaped, "text/html");
    return doc.documentElement.textContent;
  }
  const nutils = require("node:util");
  // Node.js environment: Create a virtual DOM element to handle all entities
  const textarea = new TextEncoder();
  const decoder = new TextDecoder();

  return escaped.replace(/&(#(?:x[0-9a-f]+|\d+)|[a-z]+);/gi, (_, entity) => {
    if (entity[0] === "#") {
      const code =
        entity[1].toLowerCase() === "x"
          ? parseInt(entity.slice(2), 16) : parseInt(entity.slice(1), 10);
      try {
        return decoder.decode(textarea.encode(String.fromCodePoint(code)));
      } catch {
        return `&${entity};`;
      }
    } else {
      const entities = {
        gt: ">",
        lt: "<",
        quot: '"',
        apos: "'",
        amp: "&",
        copy: "©",
        reg: "®",
        euro: "€",
        pound: "£",
        cent: "¢",
        deg: "°",
        bull: "•",
        middot: "·",
        ndash: "–",
        mdash: "—",
        lsquo: "'",
        rsquo: "'",
        sbquo: "‚",
        ldquo: '"',
        rdquo: '"',
        bdquo: "„",
        hellip: "…",
        trade: "™",
        nbsp: " ",
      };
      return entities[entity.toLowerCase()] || `&${entity};`;
    }
  });
}

module.exports = {
  do_unescape,
};
