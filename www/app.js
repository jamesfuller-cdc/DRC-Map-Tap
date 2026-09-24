(function () {
  Shiny.addCustomMessageHandler("copyShareText", function (message) {
    var text = message.text || "";
    var status = document.getElementById("copy-status");
    function succeeded() {
      if (status) status.textContent = "Copied to clipboard.";
    }
    function failed() {
      if (status) status.textContent = "Clipboard access was unavailable.";
    }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(succeeded).catch(failed);
    } else {
      var area = document.createElement("textarea");
      area.value = text;
      area.style.position = "fixed";
      area.style.opacity = "0";
      document.body.appendChild(area);
      area.select();
      try {
        if (document.execCommand("copy")) succeeded(); else failed();
      } catch (e) { failed(); }
      document.body.removeChild(area);
    }
  });
})();
