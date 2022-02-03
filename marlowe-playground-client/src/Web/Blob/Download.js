exports.downloadImpl = function (filename, blob) {
  var URL = window.URL || window.webkitURL;
  var downloadUrl = URL.createObjectURL(blob);
  var a = document.createElement("a");

  if (typeof window.navigator.msSaveBlob !== "undefined") {
    // IE workaround for "HTML7007: One or more blob URLs were revoked by closing the blob for which they were created. These URLs will no longer resolve as the data backing the URL has been freed."
    window.navigator.msSaveBlob(blob, filename);
  } else {
    if (filename) {
      // use HTML5 a[download] attribute to specify filename
      // safari doesn't support this yet
      if (typeof a.download === "undefined") {
        window.location.href = downloadUrl;
      } else {
        a.href = downloadUrl;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
      }
    } else {
      window.location.href = downloadUrl;
    }
  }
  setTimeout(() => {
    window.URL.revokeObjectURL(downloadUrl);
    a.remove();
  }, 0);

  return {};
};
