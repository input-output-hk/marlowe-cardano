"use strict";

import { bech32 } from "bech32";

export const b32 = bech32;

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
export let uiRoles = [];
export let uiNumbers = [];

const runtimeUrl = "http://192.168.0.12:13780";

var nami = null;

const delay = 5000;

export let address = "";
export let key = {};

const ada = 1000000;
export let policy = "";
export let contract = {};
export let continuations = {};

export let contractId = "";
export let contractUrl = null;
export let currentContract = {};
export let transactionUrl = null;

function setStatus(msg) {
  uiStatus.innerText = msg;
}

function setAddress(a) {
  const bytes = [];
  for (let c = 0; c < a.length; c += 2)
    bytes.push(parseInt(a.substr(c, 2), 16));
  address = bech32.encode("addr_test", bech32.toWords(bytes), 1000);
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

function submitTransaction(cborHex, url, wait) {
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
    "application/vendor.iog.marlowe-runtime.contract-tx-json",
    function (res) {
      console.log(res);
      setContract(res.resource.contractId);
      contractUrl = runtimeUrl + "/" + res.links.contract;
      const followup = function () {
        setTx(contractId.replace(/#.*$/, ""));
        enableCards(true);
      };
      setStatus("Submitting transaction.");
      submitTransaction(
        res.resource.tx.cborHex,
        contractUrl,
        waitForConfirmation(contractUrl, followup)
      );
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
    "application/vendor.iog.marlowe-runtime.apply-inputs-tx-json",
    function (res) {
      transactionUrl = runtimeUrl + "/" + res.links.transaction;
      const tx = res.resource.transactionId;
      submitTransaction(
        res.resource.tx.cborHex,
        transactionUrl,
        waitForConfirmation(transactionUrl, followup(tx))
      );
    }
  );
}

export async function makeChoice(i) {
  enableCards(false);
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
      enableCards(currentContract != null);
      if (currentContract == null) {
        setStatus("Contract complete.");
        enableStart(true);
      }
    };
  };
  applyInputs("choice", [input], followup);
}

export function enableCards(state) {
  uiRoles.forEach((e) => (e.disabled = !state));
  uiNumbers.forEach((e) => (e.disabled = !state));
  const cursor = state ? "default" : "wait";
  uiBody.style.cursor = cursor;
  uiRoles.forEach((e) => (e.style.cursor = cursor));
  uiNumbers.forEach((e) => (e.style.cursor = cursor));
  uiStart.style.cursor = cursor;
}

export function enableStart(state) {
  uiStart.disabled = !state;
}

export function initialize() {
  cardano.nami
    .enable()
    .then(function (n) {
      nami = n;
      nami
        .getChangeAddress()
        .then(function (a) {
          setAddress(a);
        })
        .catch(function (error) {
          setStatus(error);
        });
    })
    .catch(function (error) {
      setStatus(error);
    });
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
  enableCards(false);
  setStatus("Ready to create contract.");
}
