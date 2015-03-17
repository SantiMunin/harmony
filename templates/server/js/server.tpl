var express = require('express'),
    bodyParser = require('body-parser'),
    mongoose = require('mongoose'),
    app = express(),
    args = process.argv.slice(2);

app.use(bodyParser.json());

mongoose.connect(args[1]);

{{#schema}}
var {{schemaName}}Schema = new mongoose.Schema({
    {{#schemaVars}} {{varName}} : { type: {{varType}} {{#isKey}}, unique: true, index: true,
    dropDupes: true {{/isKey}} {{#isRequired}}, required: true {{/isRequired}} }, {{/schemaVars}}
});

{{schemaName}}Schema.set('toJSON', {
  transform: function (doc, ret, options) {
    {{^hasKeyField}}ret.id = ret._id{{/hasKeyField}}
    delete ret._id
    delete ret.__v
  }
});

{{^hasKeyField}}
{{schemaName}}Schema.set('toObject', {
  transform : function (doc, ret, options) {
    ret_id = ret.id
    delete ret.id
  }
});
{{/hasKeyField}}

var {{schemaName}} = mongoose.model('{{schemaName}}', {{schemaName}}Schema);

app.get('{{&schemaRoute}}', function(req, res) {
        {{schemaName}}.find(function(err, result) {
          if (err) return console.error(err);
            res.send(result);
         });
});

app.get('{{&schemaRoute}}/:id', function(req, res) {
        {{schemaName}}.find({ {{#hasKeyField}}{{keyField}}{{/hasKeyField}}{{^hasKeyField}}_id{{/hasKeyField}}: req.params.id }, function(err, result) {
          if (err) return console.error(err);
            if (result.length == 0) {
              res.status(404).send('Not Found')
              } else {
                res.send(result[0]);
              }
         });
});

app.put('{{&schemaRoute}}', function(req, res) {
  {{schemaName}}.create(new {{schemaName}}(req.body), function(err, post) {
    if (err) {
      res.json("{ \"result\": \"ERROR\", \"reason\": \"" + err + "\" }");
      return;
    }
    res.json("OK");
  });
});

app.delete('{{&schemaRoute}}/:id', function(req, res) {
  {{schemaName}}.remove({ {{#hasKeyField}}{{keyField}}{{/hasKeyField}}{{^hasKeyField}}_id{{/hasKeyField}}: req.params.id}, function(err, result) {
      if (err) { 
        res.json("ERROR");
        return;
    }});
    res.json("OK");
});

{{/schema}}

app.listen(args[0]);
