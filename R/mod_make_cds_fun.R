#' make_cds_fun UI Function
#'
#' @description An Easter egg tab. Press the button. You deserve it.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
mod_make_cds_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "cds-fun-wrapper",
    style = "position:relative; width:100%; overflow:hidden;",

    # Initial screen
    shiny::div(
      id = ns("initial"),
      style = paste(
        "position:absolute; inset:0; display:flex; align-items:center;",
        "justify-content:center; background:white; z-index:2;"
      ),
      shiny::tags$button(
        class = "btn btn-primary",
        onclick = sprintf("cdsStartFun('%s','%s')", ns("initial"), ns("stage")),
        "Make CDS Fun"
      )
    ),

    # Fun stage (hidden until button click)
    shiny::div(
      id = ns("stage"),
      style = "display:none; position:absolute; inset:0; overflow:hidden;",
      shiny::tags$button(
        onclick = sprintf(
          "cdsStopFun('%s','%s'); var t=document.querySelector('.nav-link[data-value=\"schema_browser\"]'); if(t) t.click();",
          ns("initial"),
          ns("stage")
        ),
        style = paste(
          "position:absolute; top:14px; right:14px; z-index:9999;",
          "padding:9px 16px; background:rgba(20,20,20,0.82); color:white;",
          "border:2px solid rgba(255,255,255,0.55); border-radius:8px;",
          "cursor:pointer; font-size:13px; font-weight:bold; font-family:inherit;",
          "backdrop-filter:blur(6px); box-shadow:0 2px 12px rgba(0,0,0,0.45);"
        ),
        "Stop the Fun!"
      )
    ),

    shiny::tags$style(shiny::HTML(.cds_fun_css())),
    shiny::tags$script(shiny::HTML(.cds_fun_js()))
  )
}


#' make_cds_fun Server Function
#' @noRd
mod_make_cds_fun_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}


# CSS -------------------------------------------------------------------------

.cds_fun_css <- function() {
  "
/* Zero out tab-pane padding for this tab and set height */
.tab-content > .tab-pane:has(.cds-fun-wrapper) {
  padding: 0 !important;
  overflow: hidden !important;
}
.cds-fun-wrapper {
  height: calc(100vh - 56px);
}
@keyframes cds-tumble {
  from { transform: translateX(0)                   rotate(0deg); }
  to   { transform: translateX(calc(110vw + 200px))  rotate(1440deg); }
}
@keyframes cds-tumble-rev {
  from { transform: translateX(0)                    rotate(0deg); }
  to   { transform: translateX(calc(-110vw - 200px)) rotate(-1440deg); }
}
@keyframes cds-laser-h {
  0%   { width:0;    opacity:1; }
  55%  { width:100%; opacity:1; }
  100% { width:100%; opacity:0; }
}
@keyframes cds-laser-v {
  0%   { height:0;    opacity:1; }
  55%  { height:100%; opacity:1; }
  100% { height:100%; opacity:0; }
}
@keyframes cds-laser-diag {
  0%   { opacity:0; }
  15%  { opacity:1; }
  65%  { opacity:1; }
  100% { opacity:0; }
}
@keyframes cds-float-up {
  0%   { transform:translateY(0)      scale(1)    rotate(0deg);   opacity:1; }
  100% { transform:translateY(-280px) scale(0.2)  rotate(200deg); opacity:0; }
}
@keyframes cds-confetti-fall {
  0%   { transform:translateY(-10px) rotate(0deg)   scale(1);   opacity:1; }
  85%  { opacity:0.7; }
  100% { transform:translateY(108vh) rotate(680deg) scale(0.4); opacity:0; }
}
@keyframes cds-rabbit-hop {
  0%,100% { transform:translateY(0)      scaleY(1)    scaleX(1); }
  25%     { transform:translateY(-90px)  scaleY(1.15) scaleX(0.88); }
  50%     { transform:translateY(-115px) scaleY(1.22) scaleX(0.82); }
  75%     { transform:translateY(-18px)  scaleY(0.9)  scaleX(1.12); }
}

@keyframes cds-letters-flash {
  0%   { color:#ff88ff; text-shadow:0 0 30px #ff88ff, 0 0 60px #7700dd; }
  20%  { color:#88ffff; text-shadow:0 0 30px #88ffff, 0 0 60px #0055cc; }
  40%  { color:#ffff88; text-shadow:0 0 30px #ffff88, 0 0 60px #dd6600; }
  60%  { color:#88ff88; text-shadow:0 0 30px #88ff88, 0 0 60px #006622; }
  80%  { color:#ff8888; text-shadow:0 0 30px #ff8888, 0 0 60px #cc0000; }
  100% { color:#ff88ff; text-shadow:0 0 30px #ff88ff, 0 0 60px #7700dd; }
}
"
}


# JavaScript ------------------------------------------------------------------

.cds_fun_js <- function() {
  # Emoji arrays kept in a short separate string (R's \uXXXX limit is 10k chars per literal)
  arrays_js <- "(function () {
  var S = { running:false, ints:[], tos:[], af:null, stageId:null, hue:0 };
  var LASER  = ['#ff0000','#00ff40','#00aaff','#ff00ff','#ffff00','#ff6600','#00ffff','#ff0088'];
  var CONF   = ['#ff0080','#ff4400','#ffe000','#00e080','#00cfff','#9040ff','#ff44aa','#44eeff'];
  var HEARTS = ['\u2665','\u2764','\uD83D\uDC95','\uD83D\uDC96','\uD83D\uDC97',
                '\uD83D\uDC93','\uD83D\uDC98','\uD83D\uDC9A','\uD83D\uDC9B','\uD83D\uDC99',
                '\uD83D\uDC9C','\uD83D\uDC9E'];
  var PIRATES= ['\uD83C\uDFF4','\uD83E\uDD9C','\u2693','\uD83D\uDC8E',
                '\uD83C\uDFF4','\uD83E\uDD9C','\u2693','\uD83D\uDC8E'];
  var EMOJI  = ['\uD83E\uDD84','\uD83D\uDC38','\uD83D\uDC19','\uD83E\uDD8B','\uD83D\uDC2C',
                '\uD83C\uDFB8','\uD83C\uDF55','\uD83E\uDD8A','\uD83D\uDC27','\uD83D\uDC09',
                '\uD83D\uDC7D','\uD83E\uDD16','\uD83C\uDFBA','\uD83C\uDFBB','\uD83C\uDFC6',
                '\uD83D\uDE80','\uD83C\uDF08','\uD83D\uDCAB'];
  var SPARK  = ['\u2728','\u2B50','\uD83C\uDF1F','\uD83D\uDCAB','\uD83C\uDF86','\uD83C\uDF87'];
  var HOG    = '\uD83E\uDD94';
  var BUNNY  = '\uD83D\uDC07';
  var FIRES  = ['\uD83D\uDD25','\uD83D\uDCA5','\u2728','\uD83C\uDF86'];"

  # Rest of JS — no Unicode escapes, so can be as long as needed
  paste0(
    arrays_js,
    "

  function rnd(a,b){ return a + Math.random()*(b-a); }
  function pick(a){ return a[Math.floor(Math.random()*a.length)]; }

  function mk(tag, parent, css) {
    var el = document.createElement(tag);
    el.style.cssText = css;
    el.dataset.cdsFun = '1';
    if (parent) parent.appendChild(el);
    return el;
  }

  function gone(el, ms) {
    var t = setTimeout(function(){ if (el && el.parentNode) el.remove(); }, ms);
    S.tos.push(t);
  }

  /* background */
  function bgLoop() {
    if (!S.running) return;
    S.hue = (S.hue + 0.9) % 360;
    var s = document.getElementById(S.stageId);
    if (s) {
      var h2=(S.hue+120)%360, h3=(S.hue+240)%360;
      s.style.background =
        'linear-gradient(135deg,hsl('+S.hue+',100%,78%),hsl('+h2+',100%,78%),hsl('+h3+',100%,78%))';
    }
    S.af = requestAnimationFrame(bgLoop);
  }

  /* CDS letters */
  function spawnCDS(stage) {
    mk('div', stage,
      'position:absolute; top:50%; left:50%;' +
      'transform:translate(-50%,-50%);' +
      'font-size:120px; font-weight:900; letter-spacing:0.15em;' +
      'font-family:inherit; line-height:1; user-select:none;' +
      'animation:cds-letters-flash .45s linear infinite;' +
      'pointer-events:none; z-index:12;'
    ).textContent = 'CDS';
  }

  /* hedgehog — slowed to 8–13 s, more frequent */
  function spawnHog(stage, rev) {
    var sz=rnd(60,120), top=rnd(5,72), dur=rnd(8,13);
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px; top:'+top+'%; line-height:1;' +
      'pointer-events:none; z-index:22;' +
      (rev
        ? 'left:calc(110vw + 130px); animation:cds-tumble-rev '+dur+'s linear forwards;'
        : 'left:-130px;              animation:cds-tumble '+dur+'s linear forwards;')
    );
    el.textContent = HOG;
    gone(el, (dur+.5)*1000);
  }

  /* axial lasers — unchanged speed */
  function spawnLaser(stage) {
    var col = pick(LASER);
    var glow = 'box-shadow:0 0 9px 5px '+col+'; pointer-events:none; z-index:5;';
    if (Math.random() > .45) {
      var y=rnd(3,96), goR=Math.random()>.5;
      var el = mk('div', stage,
        'position:absolute; top:'+y+'%;'+(goR?'left:0;':'right:0;')+
        'height:3px; width:0;'+
        'background:linear-gradient('+(goR?'to right':'to left')+
          ',transparent,'+col+' 30%,#fff 50%,'+col+' 70%,transparent);'+
        glow+'animation:cds-laser-h .55s ease-out forwards;'
      );
      gone(el, 650);
    } else {
      var x=rnd(3,96), goD=Math.random()>.5;
      var el2 = mk('div', stage,
        'position:absolute; left:'+x+'%;'+(goD?'top:0;':'bottom:0;')+
        'width:3px; height:0;'+
        'background:linear-gradient('+(goD?'to bottom':'to top')+
          ',transparent,'+col+' 30%,#fff 50%,'+col+' 70%,transparent);'+
        glow+'animation:cds-laser-v .55s ease-out forwards;'
      );
      gone(el2, 650);
    }
  }

  /* diagonal lasers — unchanged speed */
  function spawnDiagLaser(stage) {
    var col = pick(LASER);
    // angle avoids near-horizontal (let axial lasers handle those)
    var angle = rnd(20, 70) * (Math.random()>.5 ? 1 : -1);
    var y = rnd(5, 90);
    var el = mk('div', stage,
      'position:absolute; top:'+y+'%; left:-15%;'+
      'width:130%; height:3px;'+
      'background:linear-gradient(to right,transparent,'+col+' 20%,#fff 50%,'+col+' 80%,transparent);'+
      'box-shadow:0 0 9px 5px '+col+';'+
      'transform:rotate('+angle+'deg); transform-origin:center center;'+
      'pointer-events:none; z-index:5;'+
      'animation:cds-laser-diag .65s ease-in-out forwards;'
    );
    gone(el, 750);
  }

  /* rainbow — slowed to 5–8 s */
  function spawnRainbow(stage) {
    var ns2='http://www.w3.org/2000/svg';
    var svg=document.createElementNS(ns2,'svg');
    var r=rnd(80,170);
    svg.setAttribute('viewBox','0 0 200 110');
    svg.setAttribute('width',r*2+'px');
    svg.setAttribute('height',r+'px');
    svg.style.cssText =
      'position:absolute; left:'+rnd(10,70)+'%; top:'+rnd(20,75)+'%;'+
      'transform:translate(-50%,-100%); overflow:visible;'+
      'pointer-events:none; z-index:9;'+
      'animation:cds-float-up '+rnd(5,8)+'s ease-out forwards;';
    svg.dataset.cdsFun='1';
    ['#FF0000','#FF6600','#FFFF00','#00CC00','#0066FF','#8800CC'].forEach(function(c,i){
      var p=document.createElementNS(ns2,'path');
      var R=95-i*13;
      p.setAttribute('d','M '+(100-R)+' 100 A '+R+' '+R+' 0 0 1 '+(100+R)+' 100');
      p.setAttribute('fill','none');
      p.setAttribute('stroke',c);
      p.setAttribute('stroke-width','11');
      p.setAttribute('stroke-linecap','round');
      svg.appendChild(p);
    });
    stage.appendChild(svg);
    gone(svg, 9000);
  }

  /* rabbits — slower hop cycle */
  function spawnRabbit(stage) {
    var sz=rnd(38,68), x=rnd(2,88), dur=rnd(.75,1.15);
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px; bottom:'+rnd(0,6)+'%; left:'+x+'%;'+
      'line-height:1; pointer-events:none; z-index:16;'+
      'animation:cds-rabbit-hop '+dur+'s ease-in-out infinite;'
    );
    el.textContent = BUNNY;
    gone(el, rnd(4000,7000));
  }

  /* confetti — slower fall */
  function spawnConfetti(stage) {
    var x=rnd(0,100), sz=rnd(7,15), dur=rnd(5,9);
    var el = mk('div', stage,
      'position:absolute; top:-18px; left:'+x+'%;'+
      'width:'+sz+'px; height:'+sz+'px;'+
      'background:'+pick(CONF)+'; border-radius:'+(Math.random()>.4?'50%':'2px')+';'+
      'pointer-events:none; z-index:4;'+
      'animation:cds-confetti-fall '+dur+'s linear forwards;'
    );
    gone(el, (dur+.3)*1000);
  }

  /* flying critters — slower */
  function spawnCritter(stage) {
    var sz=rnd(42,82), top=rnd(4,74), dur=rnd(8,14), rev=Math.random()>.5;
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px; top:'+top+'%; line-height:1;'+
      'pointer-events:none; z-index:19;'+
      (rev
        ? 'left:calc(110vw + 90px); animation:cds-tumble-rev '+dur+'s linear forwards;'
        : 'left:-90px;              animation:cds-tumble '+dur+'s linear forwards;')
    );
    el.textContent = pick(EMOJI);
    gone(el, (dur+.5)*1000);
  }

  /* hearts — float up slowly */
  function spawnHeart(stage) {
    var sz=rnd(28,65), dur=rnd(2.5,5);
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px;'+
      'left:'+rnd(5,90)+'%; bottom:'+rnd(5,35)+'%;'+
      'line-height:1; pointer-events:none; z-index:14;'+
      'animation:cds-float-up '+dur+'s ease-out forwards;'
    );
    el.textContent = pick(HEARTS);
    gone(el, (dur+.2)*1000);
  }

  /* pirates — fly across slowly */
  function spawnPirate(stage) {
    var sz=rnd(50,95), top=rnd(4,74), dur=rnd(8,14), rev=Math.random()>.5;
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px; top:'+top+'%; line-height:1;'+
      'pointer-events:none; z-index:21;'+
      (rev
        ? 'left:calc(110vw + 100px); animation:cds-tumble-rev '+dur+'s linear forwards;'
        : 'left:-100px;              animation:cds-tumble '+dur+'s linear forwards;')
    );
    el.textContent = pick(PIRATES);
    gone(el, (dur+.5)*1000);
  }

  /* sparkles — slower */
  function spawnSparkle(stage) {
    var sz=rnd(15,34), dur=rnd(1.5,3);
    var el = mk('div', stage,
      'position:absolute; font-size:'+sz+'px;'+
      'left:'+rnd(0,94)+'%; top:'+rnd(0,89)+'%;'+
      'line-height:1; pointer-events:none; z-index:7;'+
      'animation:cds-float-up '+dur+'s ease-out forwards;'
    );
    el.textContent = pick(SPARK);
    gone(el, (dur+.2)*1000);
  }

  /* fire bursts — slower float */
  function spawnFireBurst(stage) {
    var cx=rnd(10,80), cy=rnd(10,78);
    for (var i=0; i<6; i++) {
      (function(i){
        var t = setTimeout(function(){
          if (!S.running) return;
          var el = mk('div', stage,
            'position:absolute; font-size:'+rnd(28,58)+'px;'+
            'left:'+(cx+rnd(-12,12))+'%; top:'+(cy+rnd(-12,12))+'%;'+
            'line-height:1; pointer-events:none; z-index:26;'+
            'animation:cds-float-up '+rnd(1.5,3)+'s ease-out forwards;'
          );
          el.textContent = pick(FIRES);
          gone(el, 3500);
        }, i*120);
        S.tos.push(t);
      })(i);
    }
  }

  /* PUBLIC */
  window.cdsStartFun = function(initialId, stageId) {
    S.stageId = stageId;
    S.running = true;
    S.hue = Math.random()*360;

    var initial = document.getElementById(initialId);
    var stage   = document.getElementById(stageId);
    if (initial) initial.style.display = 'none';
    if (!stage)  return;
    stage.style.display = 'block';

    bgLoop();
    spawnCDS(stage);

    // many hedgehogs
    spawnHog(stage,false); spawnHog(stage,true); spawnHog(stage,false);
    S.ints.push(setInterval(function(){ spawnHog(stage,false); }, 2000));
    S.ints.push(setInterval(function(){ spawnHog(stage,true);  }, 2800));

    // lasers: axial + diagonal, fast
    S.ints.push(setInterval(function(){
      if (Math.random() < .35) spawnDiagLaser(stage);
      else                     spawnLaser(stage);
    }, 170));

    // rainbows
    spawnRainbow(stage);
    S.ints.push(setInterval(function(){ spawnRainbow(stage); }, 3500));

    // rabbits
    spawnRabbit(stage); spawnRabbit(stage); spawnRabbit(stage);
    S.ints.push(setInterval(function(){ spawnRabbit(stage); }, 900));

    // confetti
    S.ints.push(setInterval(function(){
      for (var i=0;i<4;i++) spawnConfetti(stage);
    }, 80));

    // hearts — frequent
    spawnHeart(stage); spawnHeart(stage);
    S.ints.push(setInterval(function(){ spawnHeart(stage); }, 350));

    // pirates
    spawnPirate(stage);
    S.ints.push(setInterval(function(){ spawnPirate(stage); }, 1800));

    // critters
    spawnCritter(stage);
    S.ints.push(setInterval(function(){ spawnCritter(stage); }, 1800));

    // sparkles
    S.ints.push(setInterval(function(){ spawnSparkle(stage); }, 180));

    // fire bursts
    S.ints.push(setInterval(function(){ spawnFireBurst(stage); }, 2800));
  };

  window.cdsStopFun = function(initialId, stageId) {
    S.running = false;
    S.ints.forEach(clearInterval);  S.ints = [];
    S.tos.forEach(clearTimeout);    S.tos  = [];
    if (S.af) { cancelAnimationFrame(S.af); S.af = null; }

    var stage = document.getElementById(stageId);
    if (stage) {
      stage.querySelectorAll('[data-cds-fun]').forEach(function(el){ el.remove(); });
      stage.style.background = '';
      stage.style.display    = 'none';
    }
    var initial = document.getElementById(initialId);
    if (initial) initial.style.display = 'flex';
  };
})();
"
  )
}
