var ctx = famous.core.Engine.createContext();

var tpl = document.querySelector('#main');

var content = document.importNode(tpl.content, true);

console.debug("content = ", content);

ctx.add(new famous.physics.constraints.Surface({
  content: content,
  size: [200, 200],
  properties: {
    backgroundColor: 'rgb(240, 238, 233)',
    textAlign:       'center',
    padding:         '5px',
    border:          '2px solid rgb(210, 208, 203)',
    marginTop:       '50px',
    marginLeft:      '50px',
  }
}));
