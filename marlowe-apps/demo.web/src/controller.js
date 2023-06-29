"use strict";

import { bech32 } from "bech32";

const b32 = bech32;

const useNami = false;

const roles = [
  "c.marlowe",
  "e.cary",
  "f.beaumont",
  "j.lumley",
  "j.webster",
  "m.herbert",
  "w.shakespeare",
];

const nRoles = roles.length;
let uiRoles = [];
let uiNumbers = [];

const runtimeUrl = "http://192.168.0.12:13780";

var nami = null;

const delay = 5000;

let address = "";
let key = {};

const ada = 1000000;
let policy = "";
let contract = {};
let continuations = {};

export let contractId = "";
export let contractUrl = null;
export let currentContract = {};
export let transactionUrl = null;

function setStatus(msg) {
  uiStatus.innerText = msg;
}

function setAddressBytes(a) {
  const bytes = [];
  for (let c = 0; c < a.length; c += 2)
    bytes.push(parseInt(a.substr(c, 2), 16));
  setAddress(bech32.encode("addr_test", bech32.toWords(bytes), 1000));
}

function setAddress(a) {
  address = a;
  const display =
    address.substr(0, 17) + "..." + address.substr(address.length - 8);
  uiAddress.innerHTML =
    "<a href='https://preprod.cardanoscan.io/address/" +
    a +
    "' target='marlowe'>" +
    display +
    "</a>";
}

function setContract(c) {
  contractId = c;
  const display = c.substr(0, 8) + "..." + c.substr(c.length - 10);
  uiContract.innerHTML =
    "<a href='https://preprod.marlowescan.com/contractView?tab=info&contractId=" +
    contractId.replace("#", "%23") +
    "' target='marlowe'>" +
    display +
    "</a>";
}

function setTx(tx) {
  const display = tx.substr(0, 8) + "..." + tx.substr(tx.length - 8);
  uiTransaction.innerHTML =
    "<a href='https://preprod.cardanoscan.io/transaction/" +
    tx +
    "?tab=utxo' target='marlowe'>" +
    display +
    "</a>";
}

async function getCurrentContract(followup) {
  const xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (this.readyState == 4) {
      console.log({
        operation: "contract",
        status: this.status,
        response: this.responseText,
      });
      if (this.status == 200) {
        const res = JSON.parse(this.responseText);
        currentContract = res.resource.currentContract;
        followup();
      } else {
        setStatus("Failed fetching current contract.");
      }
    }
  };
  xhttp.open("GET", contractUrl);
  xhttp.setRequestHeader("Accept", "application/json");
  console.log({ operation: "contract" });
  xhttp.send();
}

function submitTransactionCli(txBody, followup) {
  const req = {
    txBody: txBody,
    key: key,
  };
  const xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (this.readyState == 4) {
      console.log({
        operation: "sign-submit",
        status: this.status,
        response: this.responseText,
      });
      if (this.status == 200) {
        const res = JSON.parse(this.responseText);
        setTimeout(function () {
          getCurrentContract(function () {
            followup(res);
          });
        }, delay);
      } else {
        setStatus("Transaction building failed.");
      }
    }
  };
  xhttp.open("POST", "/sign-submit");
  xhttp.setRequestHeader("Content-Type", "application/json");
  xhttp.setRequestHeader("Accept", "application/json");
  console.log({ operation: "sign-submit", request: req });
  xhttp.send(JSON.stringify(req));
}

function submitTransactionNami(cborHex, url, wait) {
  nami
    .signTx(cborHex, true)
    .then(function (witness) {
      const xhttp = new XMLHttpRequest();
      xhttp.onreadystatechange = function () {
        if (this.readyState == 4) {
          console.log({
            operation: "submit",
            status: this.status,
            response: this.responseText,
          });
          if (this.status == 202) {
            setTimeout(wait, delay);
          } else {
            setStatus("Transaction submission failed.");
          }
        }
      };
      xhttp.open("PUT", url);
      xhttp.setRequestHeader("Content-Type", "application/json");
      const req = {
        type: "ShelleyTxWitness BabbageEra",
        description: "",
        cborHex: witness,
      };
      console.log({ operation: "submit", request: req });
      xhttp.send(JSON.stringify(req));
    })
    .catch(function (error) {
      setStatus(error);
    });
}

function waitForConfirmation(url, followup) {
  return function () {
    const xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function () {
      if (this.readyState == 4) {
        console.log({
          operation: "wait",
          status: this.status,
          response: this.responseText,
        });
        if (this.status == 200) {
          const res = JSON.parse(this.responseText);
          if (res.resource.status == "confirmed") {
            setTimeout(followup, delay);
            setStatus("Transaction confirmed.");
            currentContract =
              res.resource.currentContract || res.resource.outputContract;
          } else if (res.resource.status == "submitted") {
            setTimeout(waitForConfirmation(url, followup), delay);
          } else {
            setStatus("Confirmation failed.");
          }
        }
      }
    };
    xhttp.open("GET", url);
    console.log({ operation: "wait" });
    setStatus("Waiting for confirmation.");
    xhttp.send();
  };
}

async function buildTransaction(operation, req, url, accept, followup) {
  const xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (this.readyState == 4) {
      console.log({
        operation: operation,
        status: this.status,
        response: this.responseText,
      });
      if (this.status == 201) {
        const res = JSON.parse(this.responseText);
        followup(res);
      } else {
        setStatus("Transaction building failed.");
      }
    }
  };
  xhttp.open("POST", url);
  xhttp.setRequestHeader("Content-Type", "application/json");
  if (accept != null) xhttp.setRequestHeader("Accept", accept);
  xhttp.setRequestHeader("X-Change-Address", address);
  console.log({ operation: operation, request: req });
  xhttp.send(JSON.stringify(req));
}

export async function createContract() {
  enableStart(false);
  enableCards(false, true);
  uiTransaction.innerText = "";
  buildTransaction(
    "create",
    {
      version: "v1",
      contract: contract,
      roles: policy,
      minUTxODeposit: 2 * ada,
      metadata: {},
      tags: {
        "5july2023": [{ revision: 2 }],
      },
    },
    runtimeUrl + "/contracts",
    useNami ? "application/vendor.iog.marlowe-runtime.contract-tx-json" : null,
    function (res) {
      console.log(res);
      setContract(res.resource.contractId);
      contractUrl = runtimeUrl + "/" + res.links.contract;
      const followup = function () {
        setTx(contractId.replace(/#.*$/, ""));
        enableCards(true, false);
      };
      setStatus("Submitting transaction.");
      if (useNami)
        submitTransactionNami(
          res.resource.tx.cborHex,
          contractUrl,
          waitForConfirmation(contractUrl, followup)
        );
      else submitTransactionCli(res.resource.txBody, followup);
    }
  );
}

async function applyInputs(operation, inputs, followup) {
  buildTransaction(
    operation,
    {
      version: "v1",
      inputs: inputs,
      metadata: {},
      tags: {
        "5july2023": [{ revision: 2 }],
      },
    },
    contractUrl + "/transactions",
    useNami
      ? "application/vendor.iog.marlowe-runtime.apply-inputs-tx-json"
      : null,
    function (res) {
      transactionUrl = runtimeUrl + "/" + res.links.transaction;
      const tx = res.resource.transactionId;
      if (useNami)
        submitTransactionNami(
          res.resource.tx.cborHex,
          transactionUrl,
          waitForConfirmation(transactionUrl, followup(tx))
        );
      else submitTransactionCli(res.resource.txBody, followup(tx));
    }
  );
}

export async function makeChoice(i) {
  enableCards(false, true);
  uiTransaction.innerText = "";
  setStatus("Submitting transaction.");
  let input = {};
  if (currentContract.when[0].then) {
    input = {
      input_that_chooses_num: parseInt(uiNumbers[i].value),
      for_choice_id: {
        choice_name: currentContract.when[0].case.for_choice.choice_name,
        choice_owner: {
          role_token: roles[i],
        },
      },
    };
  } else {
    const hash = currentContract.when[0].merkleized_then;
    input = {
      input_that_chooses_num: parseInt(uiNumbers[i].value),
      for_choice_id: {
        choice_name: currentContract.when[0].case.for_choice.choice_name,
        choice_owner: {
          role_token: roles[i],
        },
      },
      continuation_hash: hash,
      merkleized_continuation: continuations[hash],
    };
  }
  const followup = function (tx) {
    return function () {
      setTx(tx);
      enableCards(currentContract != null, false);
      if (currentContract == null) {
        setStatus("Contract complete.");
        enableStart(true);
      }
    };
  };
  applyInputs("choice", [input], followup);
}

export function enableCards(enable, wait) {
  uiRoles.forEach((e) => (e.disabled = !enable));
  uiNumbers.forEach((e) => (e.disabled = !enable));
  const cursor = wait ? "wait" : "default";
  const buttonCursor = wait ? "wait" : enable ? "default" : "not-allowed";
  uiBody.style.cursor = cursor;
  uiRoles.forEach((e) => (e.style.cursor = buttonCursor));
  uiNumbers.forEach((e) => (e.style.cursor = buttonCursor));
  uiStart.style.cursor = uiStart.disabled
    ? wait
      ? "wait"
      : "not-allowed"
    : "default";
}

export function enableStart(state) {
  uiStart.disabled = !state;
  uiStart.style.cursor = uiStart.disabled ? "not-allowed" : "default";
}

export function initialize() {
  if (useNami) {
    cardano.nami
      .enable()
      .then(function (n) {
        nami = n;
        nami
          .getChangeAddress()
          .then(function (a) {
            setAddressBytes(a);
          })
          .catch(function (error) {
            setStatus(error);
          });
      })
      .catch(function (error) {
        setStatus(error);
      });
  } else {
    fetch("data/secrets.json")
      .then((response) => response.json())
      .then((json) => {
        setAddress(json.address);
        key = json.key;
      });
  }
  fetch("data/marlowe.json")
    .then((response) => response.json())
    .then((json) => {
      policy = json.tx.roles.unCurrencySymbol;
      contract = json.tx.contract;
      continuations = Object.fromEntries(new Map(json.tx.continuations));
    });
  uiRoles = Array.from({ length: nRoles }, (v, k) =>
    document.getElementById("role" + k)
  );
  uiNumbers = Array.from({ length: nRoles }, (v, k) =>
    document.getElementById("number" + k)
  );
  uiNumbers.forEach((e) => (e.value = 100 * Math.random()));
  enableCards(false, false);
  setStatus("Ready to create contract.");
}
