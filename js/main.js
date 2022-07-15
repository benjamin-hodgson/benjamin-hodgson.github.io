function init() {
    if (window.hljs) {
        hljs.highlightAll();
    } else {
        const script = document.getElementById("highlightjs-script");
        script.onload = () => { hljs.highlightAll(); };
    }
}
if (document.readyState !== "loading") {
    init();
} else {
    window.addEventListener("DOMContentLoaded", () => { init(); });
}
