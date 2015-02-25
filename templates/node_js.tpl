var express = require('express'),
    bodyParser = require('body-parser'),
    mongoose = require('mongoose'),
    app = express(),
    args = process.argv.slice(2);

app.use(bodyParser.json());

mongoose.connect(args[1]);

{{#schema}}

var {{name}}Schema = new mongoose.Schema({
    {{#Schemavar}} {{varName}} : { type: {{varType}} {{#isKey}}, unique: true, index: true,
    dropDupes: true {{/isKey}} {{#isRequired}}, required: true {{/isRequired}} }, {{/Schemavar}}
});

var {{name}} = mongoose.model('{{name}}', {{name}}Schema);

app.get('{{&route}}', function(req, res) {
        {{name}}.find(function(err, result) {
                        if (err) return console.error(err);
                 res.send(result);
         });
});

app.get('{{&route}}/:id', function(req, res) {
  res.send({{name}}.find({ {{keyField}}: req.params.id}));
});

app.put('{{&route}}', function(req, res) {
  {{name}}.create(new {{name}}(req.body), function(err, post) {
    if (err) {
      res.json("{ \"result\": \"ERROR\", \"reason\": \"" + err + "\" }");
      return;
    }
    res.json("OK");
  });
});

app.delete('{{&route}}/:id', function(req, res) {
  {{name}}.remove({ {{keyField}}: req.params.id}, function(err, result) {
      if (err) { 
        res.json("ERROR");
        return;
    }});
    res.json("OK");
});

{{/schema}}

app.listen(args[0]);

