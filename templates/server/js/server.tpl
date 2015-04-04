var express = require('express'),
    bodyParser = require('body-parser'),
    mongoose = require('mongoose'),
    app = express(),
    args = process.argv.slice(2);

app.use(bodyParser.json());

mongoose.connect(args[1]);

{{#schema}}
var {{schemaName}}Schema = new mongoose.Schema({
{{#schemaVars}}{{#isList}}
  {{varName}} : { type: [{{#isStruct}}{{referredStruct}}Schema{{/isStruct}}{{^isStruct}}{{varType}}{{/isStruct}}]
               {{/isList}}{{^isList}}
  {{varName}} : { type: {{#isStruct}}mongoose.Schema.ObjectId, ref:'{{referredStruct}}Schema'{{/isStruct}}{{^isStruct}}{{varType}}{{/isStruct}} 
               {{/isList}} {{#isKey}}, unique: true, index: true, dropDupes: true {{/isKey}} {{#isRequired}}, required: true {{/isRequired}} {{#isEnum}}, enum: [{{/isEnum}}{{#enumValues}}'{{value}}',{{/enumValues}}{{/enumData}}{{#isEnum}}]{{/isEnum}}{{/isStruct}} },
{{/schemaVars}}
});

{{schemaName}}Schema.set('toJSON', {
  transform: function (doc, ret, options) {
    {{^hasKeyField}}ret.id = ret._id{{/hasKeyField}}
    {{#schemaVars}}{{#isHidden}}delete ret.{{varName}};{{/isHidden}}{{/schemaVars}}
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

{{/schema}}

{{#schema}}
{{#schemaRoute}}
app.get('{{&route}}', function(req, res) {
        {{schemaName}}.find(function(err, result) {
          if (err) return console.error(err);
            res.send(result);
         });
});

app.get('{{&route}}/:id', function(req, res) {
        {{schemaName}}.find({ {{#hasKeyField}}{{keyField}}{{/hasKeyField}}{{^hasKeyField}}_id{{/hasKeyField}}: req.params.id }, function(err, result) {
          if (err) return console.error(err);
            if (result.length == 0) {
              res.status(404).send('Not Found');
            } else {
              res.send(result[0]);
            }
         });
});

{{#writable}}
{{^hasKeyField}}
app.post('{{&route}}', function(req, res) {
  {{schemaName}}.create(new {{schemaName}}(req.body), function(err, obj) {
    if (err) {
      res.status(500).send(err);
      return;  
    }
    res
      .status(201)
      .json('{"id" : "'+obj._id+'"}');
   });
});
{{/hasKeyField}}

app.put('{{&route}}/:id', function(req, res) {
  {{schemaName}}.update({ {{#hasKeyField}}{{keyField}}{{/hasKeyField}}{{^hasKeyField}}_id{{/hasKeyField}} : req.params.id }, req.body, {upsert : true, runValidators : true}, function(err, result) {
    if (err) {
      res.status(500).send(err);
    } else {
      res.status(200).send();
    }
  });
});

app.delete('{{&route}}/:id', function(req, res) {
  {{schemaName}}.remove({ {{#hasKeyField}}{{keyField}}{{/hasKeyField}}{{^hasKeyField}}_id{{/hasKeyField}}: req.params.id}, function(err, result) {
      if (err) { 
        res.status(500).send(err);
        return;
    }});
    res.status(200).send();
});
{{/writable}}
{{/schemaRoute}}
{{/schema}}

app.listen(args[0]);
