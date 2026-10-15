// Highlights the lines that the address's fragment names, #L12 or #L12-L20, on the pages of
// website/source/, and brings the first into view where the page has no anchor for the range.
// A classic script, which browsers run on pages opened from disk.
"use strict";
function markLines() {
  for (const line of document.querySelectorAll(".line.marked")) line.classList.remove("marked");
  const range = /^#L(\d+)(?:-L(\d+))?$/.exec(window.location.hash);
  if (!range) return;
  const first = Number(range[1]);
  const last = range[2] ? Number(range[2]) : first;
  for (let number = first; number <= last; number++) {
    document.getElementById(`L${number}`)?.classList.add("marked");
  }
  if (!document.getElementById(window.location.hash.slice(1))) {
    document.getElementById(`L${first}`)?.scrollIntoView();
  }
}
markLines();
window.addEventListener("hashchange", markLines);
