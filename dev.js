var serve = require('koa-static');
var koa = require('koa');
var app = new koa();

app.use(serve('docs/.'));
app.listen(3000);

console.log(`=> A development server is running at http://localhost:3000`);
