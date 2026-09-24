(function () {
  function setStatus(message) {
    var status = document.getElementById("copy-status");
    if (status) status.textContent = message;
  }

  function manualCopy(text) {
    var area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.setAttribute("aria-label", "Score text to copy");
    area.style.position = "fixed";
    area.style.left = "8px";
    area.style.right = "8px";
    area.style.bottom = "12px";
    area.style.zIndex = "9999";
    area.style.padding = "10px";
    area.style.background = "white";
    area.style.color = "black";
    area.style.border = "2px solid #14211e";
    area.style.borderRadius = "6px";
    area.style.font = "16px monospace";
    area.style.whiteSpace = "pre-wrap";
    document.body.appendChild(area);
    area.focus();
    area.select();
    area.setSelectionRange(0, area.value.length);
    setStatus("Copy was blocked. The score text is selected; tap and hold to copy it.");

    window.setTimeout(function () {
      if (area.parentNode) area.parentNode.removeChild(area);
    }, 12000);
  }

  function tryLegacyCopy(text) {
    var area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.style.position = "fixed";
    area.style.top = "-1000px";
    area.style.left = "-1000px";
    document.body.appendChild(area);
    area.focus();
    area.select();
    area.setSelectionRange(0, area.value.length);

    var copied = false;
    try {
      copied = document.execCommand("copy");
    } catch (e) {
      copied = false;
    }
    document.body.removeChild(area);

    if (copied) {
      setStatus("Copied to clipboard.");
    } else {
      manualCopy(text);
    }
  }

  function copyText(text) {
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(function () {
        setStatus("Copied to clipboard.");
      }).catch(function () {
        tryLegacyCopy(text);
      });
    } else {
      tryLegacyCopy(text);
    }
  }

  // Delegation handles the button even though Shiny creates it later.
  // Starting the copy in the actual tap event is important for iOS Safari.
  document.addEventListener("click", function (event) {
    var button = event.target.closest && event.target.closest("#share");
    if (!button) return;
    var text = button.getAttribute("data-share-text") || "";
    if (text) copyText(text);
  });
})();
