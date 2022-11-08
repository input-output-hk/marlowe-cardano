const loadingWalletMessage = document.createElement("p");
loadingWalletMessage.innerHTML = "Loading wallets...";
document.body.appendChild(loadingWalletMessage);

setTimeout(() => {
  loadingWalletMessage.remove();
  const cardano = /** @type {any} */ (window).cardano;

  try {
    if (cardano === null || typeof cardano !== "object") {
      throw "No wallets installed!";
    } else {
      const wallets = Object.entries(cardano)
        .filter(([, value]) => typeof value.enable === "function")
        .map(([key]) => key);

      if (wallets.length === 0) {
        throw "No wallets installed!";
      } else {
        const walleListLabel = document.createElement("h2");
        walleListLabel.innerHTML = "Installed wallets:";
        document.body.appendChild(walleListLabel);
        const walletList = document.createElement("ul");
        document.body.appendChild(walletList);

        for (const wallet of wallets) {
          const walletListItem = document.createElement("li");
          walletList.appendChild(walletListItem);

          const exploreWalletOptions = document.createElement("a");
          exploreWalletOptions.innerHTML = wallet;
          walletListItem.appendChild(exploreWalletOptions);
        }
      }
    }
  } catch (e) {
    const noWalletMessage = document.createElement("p");
    noWalletMessage.innerHTML = /** @type {any} */ (e);
    document.body.appendChild(noWalletMessage);
  }
}, 1000);
