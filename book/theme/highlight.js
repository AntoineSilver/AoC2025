(function shikiHighlighting() {
  let shikiModule;

  window.hljs = {
    configure() {
      shikiModule = import("https://esm.sh/shiki@3.4.0");
    },
    /** @param {HTMLElement} block */
    async highlightBlock(block) {
      const lang = [...block.classList.values()]
        .map((name) => name.match(/^language-(.+)$/)?.[1])
        .filter(Boolean)[0];
      if (!lang) {
        return;
      }
      const shiki = await shikiModule;
      block.parentElement.innerHTML = await shiki.codeToHtml(block.innerText, {
        lang,
        themes: {
          light: "gruvbox-light-hard",
          dark: "catppuccin-mocha",
        },
        defaultColor: false,
      });
    },
  };
})();
