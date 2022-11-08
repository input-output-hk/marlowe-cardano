const loadingWalletMessage = document.createElement("p");
loadingWalletMessage.innerHTML = "Loading wallets...";
document.body.appendChild(loadingWalletMessage);

/** Incomplete CIP-30 wallet API type
@typedef {Object} WalletApi
@property {(...args: any[]) => any} enable
*/

/** @type {(x: any) => x is { [key: string]: any }} */
const isObject = (x) => typeof x === "object" && x !== null;

/** @type {(x: any) => x is WalletApi} */
const isWalletApi = (x) => isObject(x) && typeof x.enable === "function";

setTimeout(() => {
  loadingWalletMessage.remove();
  const cardano = /** @type {any} */ (window).cardano;

  try {
    if (!isObject(cardano)) {
      throw "No wallets installed!";
    } else {
      const wallets = Object.entries(cardano)
        .filter(([, value]) => isWalletApi(value))
        .map(([key]) => key);

      if (wallets.length === 0) {
        throw "No wallets installed!";
      } else {
        const walletListLabel = document.createElement("h2");
        walletListLabel.innerHTML = "Installed wallets:";
        document.body.appendChild(walletListLabel);
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
