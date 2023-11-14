// Taken from https://codepen.io/jonnitto/project/editor/XRPjxx

// Minimal Javascript (for Edge, IE and select box)
document.addEventListener("change", function(event) {
  let element = event.target;
  if (element && element.matches(".form-element-field")) {
    element.classList[element.value ? "add" : "remove"]("-hasvalue");
  }
});
