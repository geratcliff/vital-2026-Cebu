// carlos canto // 09/26/14  
// https://forums.adobe.com/thread/287812  
var idoc = app.activeDocument;  
var ilayer = idoc.activeLayer;  
for (i=0; i<ilayer.layers.length; i++) {  
   var sublayer = ilayer.layers[i];  
   sublayer.name = sublayer.pageItems[0].name || sublayer.pageItems[0].contents;  
}