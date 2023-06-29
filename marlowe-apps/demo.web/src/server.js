const express = require("express");
const fs = require("fs");
const { execSync } = require("child_process");
const bodyParser = require("body-parser");

const app = express();
const router = express.Router();

app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.json({}));

router.post("/sign-submit", function (req, res) {
  const body = req.body;
  fs.writeFileSync("unsigned.json", JSON.stringify(body.txBody));
  fs.writeFileSync("key.json", JSON.stringify(body.key));
  const result = execSync(
    "marlowe-cli transaction submit --tx-body-file unsigned.json --required-signer key.json --timeout 600s"
  );
  console.log(result.toString());
  res.json(result.toString());
});

//add the router
app.use("/", router);

app.use(express.static("."));

const port = process.env.PORT || 1593;
app.listen(port);

console.log(`Running at Port ${port}`);
