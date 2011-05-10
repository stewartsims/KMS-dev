/* !Maps */

/*

Map Key

  = empty
w = wall
x = deadly wall (should be found in bonus levels only!)
o = token
u = high value token
a = portal A
b = portal B
e = earthquake powerup
s = shrink curse
q = guy's start position

i = instant win

*/

var maps = [
	{
	'tokens' : 0,
	'startPosition' : '',
	'portalA' : '',
	'portalB' : '',
	'map':		
'\
wwwwwwwwwwwwwwwwwwwwwwwwwwwwww\
woooooooooowwwwwwwwoooooooooow\
wowwwowwowowwwwwwwwowowwowwwow\
wowooowwowoooooooooowowwooowsw\
wowwwowwowwwwowwwwwwwowwowwwow\
wooooooo              ooooooow\
wwwwwoww wwwwwwww www wwowwwww\
wwwwwoww woooooooooow wwowwwww\
a        woooooooooow        b\
wwwwwoww woooooooooow wwowwwww\
wwwwwoww wwwwwwwwwwww wwowwwww\
wooooooo     wwww     ooooooow\
wowowwwwwwww wwww wwwwwwwwowow\
wewoooooooooooooooooooooooowow\
wowwwwwwowwwwwwwwwwwwowwwwwwow\
woooooooooooooqoooooooooooooow\
wwwwwwwwwwwwwwwwwwwwwwwwwwwwww'
	},
	{
	'tokens' : 0,
	'startPosition' : '',
	'portalA' : '',
	'portalB' : '',
	'map' :
'\
xxxxxxxxxxxx      xxxxxxxxxxxx\
x          x      x          x\
x uuuuuuuu xxxxxxxx uuuuuuuuux\
x u      u        x uxxxxxx ux\
x uuuuuuuuuuuuuuu x ux    x ux\
x u             u x uxuuu x ux\
x u xxxxxxxxxx  u x uxuxu x ux\
x u x           u xu xuxu x ux\
x u x           u xu xuxu xu x\
x u x  wwwww    u xu xxxu xu x\
x u x        x  u xuuuuuu xu x\
x u x        x  u x       xu x\
x u xxxxxxxxxx  u xxxxxxxxxu x\
x u             u   uuuuu  u x\
x uuuu      uuuuu  uwwwwwuuu x\
x        q          uuuuu    x\
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
	}
];

for (var i in maps)
{
	// convert the human-readable definiton above into a single line
	maps[i].map = maps[i].map.replace("\n", '');

	// count number of pickupable tokens in the map
	maps[i].tokens = 0;
	if (maps[i].map.match(/o/g) != null)
		maps[i].tokens += maps[i].map.match(/o/g).length;
	if (maps[i].map.match(/u/g) != null)
		maps[i].tokens += maps[i].map.match(/u/g).length;
	if (maps[i].map.match(/e/g) != null)
		maps[i].tokens += maps[i].map.match(/e/g).length;
	if (maps[i].map.match(/s/g) != null)
		maps[i].tokens += maps[i].map.match(/s/g).length;
	
	maps[i].startPosition = maps[i].map.indexOf('q');
	maps[i].portalA = maps[i].map.indexOf('a');
	maps[i].portalB = maps[i].map.indexOf('b');
		
	// convert to array
	maps[i].map = maps[i].map.split('');
}

var mapInfo = [],
	mapTileElements = [],
	mapClass = '';
	
const
	tileSize = 16,
	tilesX = 30,
	tilesY = 17;

function makeMap(mapIndex)
{
	mapClass = 'map' + mapIndex;
	stage.className = mapClass;
	
	// destroy current tiles, if any
	if (tiles.innerHTML != '')
		tiles.innerHTML = '';

	for (var i = 0; i < mapInfo.map.length; i++)
	{
		var tile = document.createElement('div');
		tile.className = 'tile';
		mapTileElements[i] = tile;
/* 		tile.innerText = i; */
		
		var coords = tileToCoords(i);
		tile.style.left = coords.x;
		tile.style.top  = coords.y;
		tile.className += (' ' + mapInfo.map[i]);

		tiles.appendChild(tile);
	}
}

function coordToTile(cX, cY)
{
	var tile = new Object();
	tile.x = Math.round(cX / tileSize);
	tile.y = Math.round(cY / tileSize);
	tile.index = (tile.y * tilesX) + tile.x;
	
	return tile;
}

function tileToCoords(index)
{
	var coords = new Object();
	coords.y = Math.floor(index / tilesX) * tileSize;
	coords.x = (index - (Math.ceil(Math.ceil(coords.y / tileSize) * tilesX))) * tileSize;
	
	return coords;
}

/* !Guy */

function Guy(id, containerName, startTile)
{
	// init properties

	this.direction = this.newDirection = 'E'; // newDirection is the next valid turn to make
	this.tile = startTile;
	this.angle = 0;	
	this.flipped = false;

	container = document.getElementById(containerName);

	// (re)create HTML element for guy
	guyElement = document.createElement('img');
	guyElement.src = 'images/guy1.gif';
	guyElement.id = id;
/* 	guyElement.className = 'blink'; */
	container.appendChild(guyElement);	
	
	// start position
	this.transform(guyElement, tileToCoords(this.tile).x, tileToCoords(this.tile).y, this.angle, false);
}

Guy.prototype.transform = function(element, deltaX, deltaY, rotation, flip)
{		
	if (flip)
	{
		if (element.src.indexOf('lip.gif') < 0)
			element.src = 'images/guyFlip.gif';
	}
	else
	{
		if (rotation == 180)
		{
			if (element.src.indexOf('2.gif') < 0)
				element.src = 'images/guy2.gif';
		}
		else
		{
			if (element.src.indexOf('1.gif') < 0)
				element.src = 'images/guy1.gif';
		}
	}

	if (rotation == 180)
		rotation = 0;

	element.style.webkitTransform = 'translate(' + deltaX + 'px,' + deltaY + 'px) rotate(' + rotation +'deg)';
}

/* !Properties */

Guy.prototype.direction = '';
Guy.prototype.newDirection = '';
Guy.prototype.tile = 0;
Guy.prototype.nextTile = '';
Guy.prototype.angle = 0;
Guy.prototype.flipped = false;

/* !Methods */

Guy.prototype.walk = function(moveWithoutAnimation)
{
	if (gameState != 'playBonus' && (this.tile == chef.tile || this.tile == chef2.tile))
		gameDie();

	if (moveWithoutAnimation == null)
		moveWithoutAnimation = false;
	
	// is there a new direction and can we move that way?
	if (this.direction != this.newDirection)
	{
		var newDirectionTileIndex = nextTileIndex(this.tile, this.newDirection);
		if (isPassableTile(newDirectionTileIndex))
		{
			this.flipped = isFlippedDirection(this.direction, this.newDirection);
			// the tile in the new direction is open, so we can move to it
			this.direction = this.newDirection;
		}
	}
	else
	{
		this.flipped = false;
	}

	switch (this.direction)
	{
		case 'E' :
			this.angle = 0;
			break;
		case 'W' :
			this.angle = 180;
			break;
		case 'S' :
			this.angle = 90;
			break;
		case 'N' :		
			this.angle = -90;
	}
	
	this.nextTile = nextTileIndex(this.tile, this.direction);

	var canMove = isPassableTile(this.nextTile);
	moveWithoutAnimation = false;
			
	switch(mapInfo.map[this.nextTile])
	{
		case 'w' :
/* 			canMove = false; */
			break;
			
		case 'o' :
			pickUpToken(this.nextTile);
			break;

		case 'u' :
			pickUpToken(this.nextTile);
			break;


		// powerups, bad surprises
		case 'e' :
			pickUpToken(this.nextTile);
			startEarthquake();
			break;

		case 's' :
			pickUpToken(this.nextTile);
			startShrink();
			break;
			
		// teleports
		case 'a' :
			moveWithoutAnimation = true;
			this.nextTile = mapInfo.portalB;
			break;
		case 'b' :
			moveWithoutAnimation = true;
			this.nextTile = mapInfo.portalA;
			break;

		case 'x' :
			gameBonusFail();
			break;

		case 'i' :
			gameWin();
			break;
	}
	
	// Move
	if (canMove)
	{
		this.tile = this.nextTile;
		if (moveWithoutAnimation)
			guyElement.style.webkitTransition = 'none';	
		
		// execute the move	
		this.transform(guyElement, tileToCoords(this.nextTile).x, tileToCoords(this.nextTile).y, this.angle, this.flipped);
		
		if (moveWithoutAnimation){		
			setTimeout("guyElement.style.webkitTransition = '-webkit-transform " + levelSpeed  + "ms linear';", 80); // put this in a constant or GET RID OF!
		}
	}
}

Guy.prototype.changeDirection = function(direction)
{
	this.newDirection = direction;
};


/* !Chef */

chefElements = [];

function Chef(id, containerName, startTile, startDirection, serial)
{
	// init properties

	this.direction = this.newDirection = startDirection; // newDirection is the next valid turn to make
	this.tile = startTile;
	this.angle = 0;	
	this.flipped = false;
	this.cycle = 0;
	this.serial = serial;

	container = document.getElementById(containerName);

	// (re)create HTML element for chef
	chefElements[this.serial] = document.createElement('img');
	chefElements[this.serial].src = 'images/chef' + this.serial + '-1.gif';
	chefElements[this.serial].id = id;
	container.appendChild(chefElements[this.serial]);	
	
	// start position
	this.transform(chefElements[this.serial], tileToCoords(this.tile).x, tileToCoords(this.tile).y, this.angle, false);
}

/* !Properties */

Chef.prototype.direction = '';
Chef.prototype.newDirection = '';
Chef.prototype.tile = 0;
Chef.prototype.nextTile = '';
Chef.prototype.angle = 0;
Chef.prototype.flipped = false;
Chef.prototype.cycle = 0;
Chef.prototype.serial = 0;

/* !Methods */

Chef.prototype.walk = function()
{
	if (this.tile == guy.tile)
		gameDie();

	// can we home in on the target?
	var hunterDirection = directionToward(tileToCoords(this.tile).x, tileToCoords(this.tile).y, tileToCoords(guy.tile).x, tileToCoords(guy.tile).y);

	this.cycle++;
	
	if (isPassableTile(nextTileIndex(this.tile, hunterDirection), true) && this.cycle % Math.floor(homeInFrequency * Math.random(2) + 1) == 0) // home in only every n turns, to prevent getting stuck?
	{
		this.newDirection = hunterDirection;
		this.cycle = 0;
	}
	else
	{
		// we can't home in on the target directly; where all can we go?
		var accessibleTiles = [];
		// add possible directions to the list; exclude the 180 move
		if (isPassableTile(nextTileIndex(this.tile, 'E'), true) && this.direction != 'W')
			accessibleTiles.push('E');
		if (isPassableTile(nextTileIndex(this.tile, 'W'), true) && this.direction != 'E')
			accessibleTiles.push('W');
		if (isPassableTile(nextTileIndex(this.tile, 'N'), true) && this.direction != 'S')
			accessibleTiles.push('N');
		if (isPassableTile(nextTileIndex(this.tile, 'S'), true) && this.direction != 'N')
			accessibleTiles.push('S');
			
		// pick a random accessible tile if we have options	
		if (accessibleTiles.length > 0)
		{
			this.newDirection = accessibleTiles[Math.round(Math.random() * (accessibleTiles.length - 1))];
		}
		else
		 // if you get stuck and can't do anything else, do a 180
			this.newDirection = getFlippedDirection(this.direction);
	}
		
	this.flipped = isFlippedDirection(this.direction, this.newDirection);

	switch (this.newDirection)
	{
		case 'E' :
			this.angle = 0;
			break;
		case 'W' :
			this.angle = 180;
			break;
		case 'S' :
			this.angle = 90;
			break;
		case 'N' :		
			this.angle = -90;
	}
	
	this.nextTile = nextTileIndex(this.tile, this.newDirection);	
	this.tile = this.nextTile;
	this.direction = this.newDirection;
	
	// execute the move	
	this.transform(chefElements[this.serial], tileToCoords(this.nextTile).x, tileToCoords(this.nextTile).y, this.angle, this.flipped);	
}

Chef.prototype.transform = function(element, deltaX, deltaY, rotation, flip)
{		
	if (flip && element.src.indexOf('flip.gif') < 0)
		element.src = 'images/chef' + this.serial + '-flip.gif';
	else
	{
		if (rotation == 180)
		{
			if (element.src.indexOf('-2.gif') < 0)
				element.src = 'images/chef' + this.serial + '-2.gif';
		}
		else
		{
			if (element.src.indexOf('-1.gif') < 0)
				element.src = 'images/chef' + this.serial + '-1.gif';
		}
	}

	if (rotation == 180)
		rotation = 0;

	element.style.webkitTransform = 'translate(' + deltaX + 'px,' + deltaY + 'px) rotate(' + rotation +'deg)';
}


function pickUpToken(index)
{
	var tokenValue = 0;
	if (isTokenTile(index))
	{	
		tokens++;
		if (mapInfo.map[index] == 'o')
			tokenValue = tokenScore;
		else if (mapInfo.map[index] == 'u')
			tokenValue = tokenBonusScore;
			
		scoreInc(tokenValue);
		
		// Victory?
		if (tokens >= tokensToWin)
			gameWin();
			
		// clear tile
		mapInfo.map[index] = ' ';
/* 		tiles.removeChild(mapTileElements[index]); // performance problem! */
/* 		mapTileElements[index].className = ' '; // still slow! */
		mapTileElements[index].style.visibility = "hidden";
	}
}

function scoreInc(points)
{
	levelScore += points;
	score += points;
	setScoreMonitor();
}

function checkHighScore()
{
	if (hiScore < score)
	{
		hiScore = score;
	}

	setHiScoreMonitor();
	// blink
/* 	hiScoreMonitor.className = 'blink monitor'; */
/* 	setTimeout("hiScoreMonitor.className = 'subtle monitor'; ", blinkDuration); */
}

function tallyLevelScore(skipTimeBonus)
{
	if (skipTimeBonus == null)
		skipTimeBonus = false;

	var timeScore = 0,
		totalScore = 0;

	labelNoticeScoreTokens.set(levelScore);
	
	// bouns rounds and deaths don't get a time score bonus
	if (!skipTimeBonus)
	{		
	// seconds under 45, squared, in 5-point increments
		if (levelTime.getMinutes() < 1) 		
			timeScore = Math.ceil(Math.pow(timeScoreThreshold - levelTime.getSeconds(), 2) / timeScoreIncrement) * timeScoreIncrement;
		
		labelNoticeScoreTime.set(timeScore);
		
		score = score + timeScore;
	}
	
	labelNoticeScoreLevel.set(levelScore + timeScore);
	labelNoticeScoreTotal.set(score);
	
	checkHighScore();
	
	// reset for next level
	levelScore = 0;
}

function setLivesMonitor(shouldBlink)
{
	if (shouldBlink)
	{
		livesMonitor.className = 'blink monitor';
		setTimeout("livesMonitor.className = 'monitor'; livesMonitor.style.width = lives * livesMonitorLifeWidth + 'px';", blinkDuration);
	}
	else
	{
		livesMonitor.style.width = lives * livesMonitorLifeWidth + 'px';	
	}
}

function setLevelMonitor()
{
	labelLevel.set(level);
}

function setScoreMonitor()
{
	labelScore.set(score);
}
function setHiScoreMonitor()
{
	labelHiScore.set(hiScore);
}


function startEarthquake()
{
	isAnimatedEarthquake = true;
	stage.className += ' earthquake ';
	setTimeout('stage.className = mapClass; isAnimatedEarthquake = false; //startChefs()', 5000);
/* 	stopChefs(); */
}

function startShrink()
{
	isAnimatedShrink = true;
	stage.className += ' shrink ';	
	setTimeout('stage.className = mapClass; isAnimatedShrink = false;', 5000);}

// this function does not check for stage bounds; it assumes an infinite stage
function nextTileIndex(index, direction)
{
	switch(direction)
	{
		case 'E' :
			return index + 1;
		case 'W' :
			return index - 1;
		case 'S' :
			return index + tilesX;
		case 'N' :
			return index - tilesX;
	}
	return false;
}

function isPassableTile(index, isChef)
{
	switch (mapInfo.map[index])
	{
		case 'x' :
			return false;
		case 'w' :
			return false;
	}
	// chefs can't enter the room. 197 is the entrance tile
	if (index == 197 && isChef)
		return false;
		
	return true;
}

function isTokenTile(index)
{
	switch (mapInfo.map[index])
	{
		case 'o' :
			return true;
		case 'e' :
			return true;
		case 's' :
			return true;
		case 'u' :
			return true;
	}
	return false;
}

function directionToward(hunterX, hunterY, preyX, preyY)
{
	var offsetX = hunterX - preyX;
	var offsetY = hunterY - preyY;
	
	if (Math.abs(offsetX) >= Math.abs(offsetY))
	{
		if (offsetX > 0)
			return 'W';
		else
			return 'E';
	}
	else
	{
		if (offsetY > 0)
			return 'N';
		else
			return 'S';	
	}
}


function isFlippedDirection(direction1, direction2)
{
	if (
		(direction1 == 'E' && direction2 == 'W') ||
		(direction1 == 'W' && direction2 == 'E')
	)
		return true;
	return false;
}

function getFlippedDirection(direction)
{
	switch (direction)
	{
		case 'E':
			return 'W';
		case 'W':
			return 'E';
		case 'N':
			return 'S';
		case 'S':
			return 'N';
		default:
			return 'E'; // in case of invalid direction input
	}
}

function clockInc()
{
	levelTime.setSeconds(levelTime.getSeconds()+1);
}


/* !Start level */

function startLevel(isRestart)
{
	if (isRestart == null)
		isRestart = false;

	welcome.style.display = 'none';
	game.style.display = 'block';

	// what map are we playing?
	if (!isRestart)
		mapInfo = copy(maps[0]);
		
	tokensToWin = mapInfo.tokens;

	// reset level
	levelSpeed = Math.floor(minLevelSpeedDelay - (level * levelSpeedIncreaseFactor));
	if (levelSpeed < 0)
		levelSpeed = 45; // min. delay
/* 	logAlert('level speed: ' + levelSpeed); */
	
	if (!isRestart)
		tokens = 0;
	
	levelTime = null; levelTime = new Date(0);
	setLevelMonitor();
	setLivesMonitor(false);
	setScoreMonitor();
	setHiScoreMonitor();

	// build map on the page
	if (!isRestart)	
		makeMap(0);
	
	// spawn characters
	guy = new Guy('guy', 'stage', mapInfo.startPosition);
	guyElement.style.webkitTransitionDuration = levelSpeed + 'ms';
	
	chef = new Chef('chef', 'stage', 155, 'E', 0);
	chefElements[0].style.webkitTransitionDuration = (levelSpeed + chefSpeedHandicap) + 'ms';

	chef2 = new Chef('chef2', 'stage', 175, 'W', 1);
	chefElements[1].style.webkitTransitionDuration = (levelSpeed + chefSpeedHandicap) + 'ms';
	
	// aaaaand action!
	
	// ... once we're sure we're ready for takeoff, that is
	if (gameState == 'launchCleared' || gameState == 'play')
	{
		setTimeout('startTimers();', blinkDuration) // start the action with a 1-second delay, to let the player survey the situation
		gameState = 'play';
	}
}

function startBonusLevel()
{
	welcome.style.display = 'none';
	game.style.display = 'block';

	// what map are we playing?
	mapInfo = copy(maps[1]);

	tokens = 0;		
	tokensToWin = mapInfo.tokens;

	// reset level
	levelSpeed = Math.floor(minLevelSpeedDelay - (level * levelSpeedIncreaseFactor));
	if (levelSpeed < 0)
		levelSpeed = 45; // min. delay
/* 	logAlert('level speed: ' + levelSpeed); */
	
	setLevelMonitor();
	setLivesMonitor(false);
	setScoreMonitor();
	setHiScoreMonitor();

	// build map on the page
	makeMap(1);
	
	// spawn characters
	guy = new Guy('guy', 'stage', mapInfo.startPosition);
	guyElement.style.webkitTransitionDuration = levelSpeed + 'ms';
		
	// aaaaand bonus action!

	gameState = 'playBonus';
	setTimeout('startTimers();', blinkDuration) // start the action with a 1-second delay, to let the player survey the situation
}

function stopLevel()
{
	stopTimers();
	
	// clean out old objects if this is a re-play
	if (document.getElementById('guy'))
	{
		stage.removeChild(document.getElementById('guy'));
		guy = null;
	}
	if (document.getElementById('chef'))
	{
		stage.removeChild(document.getElementById('chef'));
		chef = null;
	}
	if (document.getElementById('chef2'))
	{
		stage.removeChild(document.getElementById('chef2'));
		chef2 = null;
	}

}

/* !Game flow */

function gameWin()
{
	stopTimers();
	stopAnimations();
	setTimeout('stopLevel();', 1100);

	// show notice
	setTimeout("notice.style.display = 'block'; notice.className = 'win';", 1000);
	setTimeout("notice.style.opacity = 1;", 1001);
		
	level++;
	
	tallyLevelScore();
		
	// extra life every 10000 points
	if (Math.floor(score / extraLifeEveryPoints) > Math.floor(previousScore / extraLifeEveryPoints) && lives < 3)
	{
		lives++;
		setLivesMonitor();
	}
	
	// now that we're done calculating with previous score, we can set it to the new score
	previousScore = score;
	
	gameSave();	

	// dismiss notice and continue
/* 	notice.ontouchend = bonusAnnounce; */
	notice.ontouchend = function()
	{
		notice.style.opacity = 0;
		notice.style.display = 'none';
		notice.className = '';	
	
		gameState = 'play';
		startLevel();
	}
}

function bonusAnnounce()
{
	notice.ontouchend = function() {};
	notice.className = 'bonusround';	

	// fade out notice
	setTimeout("notice.style.display = 'none'; startBonusLevel()", 1000);
}

function gameBonusFail()
{
	stopTimers();
	stopAnimations();
	setTimeout('stopLevel();', 1100);
	
	// show notice
	setTimeout("notice.style.display = 'block'; notice.className = 'bonusscore';", 1000);
	setTimeout("notice.style.opacity = 1;", 1001);

	tallyLevelScore(true);

	// extra life every 10000 points
	if (Math.floor(score / extraLifeEveryPoints) > Math.floor(previousScore / extraLifeEveryPoints) && lives < 3)
	{
		lives++;
		setLivesMonitor();
	}

	// now that we're done calculating with previous score, we can set it to the new score
	previousScore = score;

	gameSave();

	// dismiss notice and continue
	notice.ontouchend = function() {
		notice.style.opacity = 0;
		notice.style.display = 'none';
		notice.className = '';	
	
		// back to normal levels
		gameState = 'play';
		startLevel();
	};
}

function gameDie()
{
	stopTimers();
	stopAnimations();
	guyElement.className = 'blink';
	setTimeout('stopLevel();', 3000);
	
	lives--;
	setLivesMonitor();

	checkHighScore();
	gameSave();
	
	if (lives == 0)
	{
		setTimeout('gameOver();', 3000);
	}
	else
	{
		// restart the same level
		setTimeout('startLevel(true);', 3000);
	}	
}

function gameOver()
{
	// show notice
	setTimeout("notice.style.display = 'block'; notice.className = 'gameover';", 1000);
	setTimeout("notice.style.opacity = 1;", 1001);
		
	tallyLevelScore(true);

	score = 0;
	lives = 3;
	level = 1;
	
	gameSave();
	
	// dismiss notice and continue
	notice.ontouchend = function() {
		notice.style.opacity = 0;
		notice.style.display = 'none';
		notice.className = '';	
		showWelcome();
	};

}

function gamePause()
{
	stopTimers();
	stopAnimations();
}

function gameContinue()
{
	hideMenu();
	startTimers();
	// resume animations
	if (isAnimatedEarthquake)
		startEarthquake();
	if (isAnimatedShrink)
		startShrink();
}

function gameContinueSaved()
{
	hideMenu();
	gameState = 'launchCleared';
	startLevel();
}

function gameNew(newLevel)
{
	level = newLevel;
	score = 0;
	lives = 3;

	stopLevel();
	hideMenu();
	gameState = 'launchCleared';
	startLevel(); //start from scratch
}

function gameSave()
{
	logAlert('previous - hiScore: ' +  localStorage.hiScore + '; score: ' +  localStorage.score + '; level: ' +  localStorage.level + '; lives: ' +  localStorage.lives);
	logAlert('memory - hiScore: ' +  hiScore + '; score: ' + score + '; level: ' +  level + '; lives: ' +  lives);
	localStorage.hiScore = hiScore;
	localStorage.score = score;
	localStorage.level = level;
	if (lives > 0)
		localStorage.lives = lives;	
	logAlert('new: hiScore: ' +  localStorage.hiScore + '; score: ' +  localStorage.score + '; level: ' +  localStorage.level + '; lives: ' +  localStorage.lives);
}

function stopAnimations()
{
	setTimeout('stage.className = mapClass; isAnimatedEarthquake = false; isAnimatedShrink = false;', 1);
}

function startTimers()
{
	if (gameState != 'playBonus')
		startChefs();
	tGuy   = window.setInterval("guy.walk(true)", levelSpeed);
	tClock = window.setInterval('clockInc()', 1000);
}

function stopTimers()
{
	if (gameState != 'playBonus')
		stopChefs();
	window.clearInterval(tGuy);
	window.clearInterval(tClock);
}

function startChefs()
{
	tChef   = window.setInterval("chef.walk(true)", levelSpeed + chefSpeedHandicap);
	tChef2   = window.setInterval("chef2.walk(true)", levelSpeed + chefSpeedHandicap);
}

function stopChefs()
{
	window.clearInterval(tChef);
	window.clearInterval(tChef2);
}


/* !Config */

const livesMonitorLifeWidth = 22;

const blinkDuration = 1500; // length of a standard blink animation
const extraLifeEveryPoints = 10000;
const bonusLevelEveryLevels = 2;
const timeScoreThreshold = 60;

const levelSpeedIncreaseFactor = .85; // each level is faster by this many ms
const minLevelSpeedDelay = 120;

const chefSpeedHandicap = 85, // higher values mean lower speed
      homeInFrequency = 5; // tis is how often in their walk chefs will try homing in on Guy; lesser number does NOT mean a smarter chef, as they can get stuck by homing in too much!

var hiScore = 0,
	level = 1,
	lives = 3,
	score = 0,
	
	tokens = 0,
	previousScore = 0,
	levelScore = 0,
	levelTime = new Date(0);

var tokenScore = 5,
	tokenBonusScore = 10,
    timeScoreIncrement = 5;

var levelSpeed = 100, // default
	tokensToWin = 0;

var isAnimatedEarthquake = false,
	isAnimatedShrink = false;
	
var startPosition = tileToCoords(mapInfo.startPosition);

var guy, guyElement;

// timers
var tGuy, tChef, tChef2, tClock;

var gameState = '';


/* !Initialize */

function preInit()
{
	// Browser check
	// Look for iPhone OS
	if (navigator.appVersion.indexOf('iPhone OS ') < 0)
	{
		document.getElementById('menu').style.display = 'none';
		document.getElementById('welcome').style.display = 'none';
		document.getElementById('game').style.display = 'none';
		document.getElementById('desktop').style.display = 'block';
	}
	else
	{
		// We have an iPhone!
		
		// Event handlers
		document.body.ontouchstart = function(e) { e.preventDefault(); };

		if (!window.navigator.standalone)
		{
			// not running as an installed app					
			document.getElementById('welcome').style.display = 'none';
			document.getElementById('game').style.display = 'none';
			document.getElementById('install').style.display = 'block';
		}
		else
		{			
			window.onorientationchange = updateOrientation;
			
			// installed app on an iPhone, we're golden!
			// preload images
			loader = new ImageLoader(initializeGame); // function to call when all is loaded
			// set the imageUrls
			loader.setImageUrls(requiredImages);
			// get it to load!
			loader.startLoading();
		}
	}	

}

function preInit2()
{
	// Fake an iphone-app-installed launch
	loader = new ImageLoader(initializeGame); // function to call when all is loaded
	// set the imageUrls
	loader.setImageUrls(requiredImages);
	// get it to load!
	loader.startLoading();
}


// This runs first when the page loads. It should account for all global elements and resources
function initializeGame()
{
	// DOM elements shorthands
	// stage
	shrinkContainer	= document.getElementById('shrinkContainer');
	game = document.getElementById('game'); 
	stage = document.getElementById('stage'); 
	tiles = document.getElementById('tiles');
	scoreBar = document.getElementById('scoreBar'); 
		
	// menu
	menu = document.getElementById('menu');
	btnContinue = document.getElementById('btnContinue');
	btnNew = document.getElementById('btnNew');
	btnAbout = document.getElementById('btnAbout');

	// welcome
	welcome = document.getElementById('welcome');
	btnWelcomeContinue = document.getElementById('btnWelcomeContinue');

	notice = document.getElementById('notice');

	game.ontouchstart = stageTouchStart;
	game.ontouchmove = stageTouchMove;
	game.ontouchend = stageTouchEnd;

	scoreMonitor = document.getElementById('scoreMonitor'); 
	hiScoreMonitor = document.getElementById('hiScoreMonitor'); 
	levelMonitor = document.getElementById('levelMonitor'); 
	livesMonitor = document.getElementById('livesMonitor'); 

	noticeRotate = document.getElementById('noticeRotate');

	// Add labels
	labelHiScore = new labelCounter('hiScoreMonitor', 6);
	labelScore   = new labelCounter('scoreMonitor', 6);
	labelLevel   = new labelCounter('levelMonitor', 2);

	noticeScoreTokens = document.getElementById('noticeScoreTokens'); 
	noticeScoreTime   = document.getElementById('noticeScoreTime'); 
	noticeScoreLevel  = document.getElementById('noticeScoreLevel'); 
	noticeScoreTotal  = document.getElementById('noticeScoreTotal'); 

	labelNoticeScoreTokens = new labelCounter('noticeScoreTokens', 6);
	labelNoticeScoreTime   = new labelCounter('noticeScoreTime', 6);
	labelNoticeScoreLevel  = new labelCounter('noticeScoreLevel', 6);
	labelNoticeScoreTotal  = new labelCounter('noticeScoreTotal', 6);
	
	// Load saved games and data
	hiScore = localStorage.hiScore != null ? parseInt(localStorage.hiScore) : hiScore;
	score   = localStorage.score   != null ? parseInt(localStorage.score)   : score;
	level   = localStorage.level   != null ? parseInt(localStorage.level)   : level;
	lives   = localStorage.lives   != null ? parseInt(localStorage.lives)   : lives;
	
	assessContinue();
	
	logAlert('hiScore: ' +  localStorage.hiScore + '; score: ' +  localStorage.score + '; level: ' +  localStorage.level + '; lives: ' +  localStorage.lives);
	
	gameState = 'launch';
/* 	iPhone assumes all apps are launched in landscape mode (!
	We have to do this with a slight delay
*/
	setTimeout('updateOrientation()', 50);
}

function assessContinue()
{
	if (!score || score < 1)
	{
		// first or clean game, nothing to continue
		welcome.className = 'new';
	}
}

/* !Swipe detection */
var
	stageTouchStartX, stageTouchEndX, stageStartX,
	stageTouchStartY, stageTouchEndY, stageStartY,
	stageShiftX = 0, stageShiftY = 0;
	
var
	swipeFloor = 20, // min. length of swipe for it to count
	swipeRatio  = 3; // the swipe can only travel amount/this on the other axis to count
	
	
function stageTouchStart(event)
{
	stageTouchStartX = stageTouchEndX = event.touches[0].screenX;
	stageStartX = stageShiftX;
	stageTouchStartY = stageTouchEndY = event.touches[0].screenY;
	stageStartY = stageShiftY;
}

function stageTouchMove(event)
{
	event.preventDefault();

	// difference between where we are in mid-move and where we started
	var moveAmountX = event.touches[0].screenX - stageTouchStartX;	
	var moveAmountY = event.touches[0].screenY - stageTouchStartY;
	// reset this in case we change direction in mid-move
	stageTouchStartX = stageTouchStartX + moveAmountX;
	stageTouchEndX = event.touches[0].screenX;
	stageTouchStartY = stageTouchStartY + moveAmountY;
	stageTouchEndY = event.touches[0].screenY;
	
	// Get actual shift
	stageShiftX = stageShiftX + moveAmountX;
	stageShiftY = stageShiftY + moveAmountY;
	
	// Assess the swipe	
	if (Math.abs(stageShiftX) > swipeFloor && Math.abs(stageShiftY) < (Math.abs(stageShiftX) / swipeRatio))
	{
		if (stageShiftX > 0)
			guy.changeDirection('E');
		else
			guy.changeDirection('W');
	}
	else if (Math.abs(stageShiftY) > swipeFloor && Math.abs(stageShiftX) < (Math.abs(stageShiftY) / swipeRatio))
	{
		if (stageShiftY > 0)
			guy.changeDirection('S');
		else
			guy.changeDirection('N');
	}
	else
	{
		// invalid swipe; no clear direction was detected
	}
}

function stageTouchEnd()
{
	// reset all
	stageTouchStartX = 0;
	stageTouchStartY = 0;
	stageShiftX = 0;
	stageShiftY = 0;
}

/* !Menu */

function showMenu()
{
	about.style.display = 'none';
	menu.style.display = 'block';
	setTimeout("menu.style.opacity = 1;", 1);
	gamePause();
	gameState = 'menu';	
}

function hideMenu()
{
	menu.style.opacity = 0;
	setTimeout("menu.style.display = 'none'; ", 300);
/* 	gameContinue(); */
}

function showAbout()
{
	menu.style.display = 'none';
	about.style.display = 'block';
}

function menuBack()
{
	about.style.display = 'none';

	if (gameState == 'menu')
	{
		showMenu();
	}
	else
	{
		showWelcome();
	}
		
}

function showWelcome()
{
	assessContinue();
	game.style.display = 'none';
	menu.style.display = 'none';
	welcome.style.display = 'block';
}


/// It's near-impossible to implement a CSS-only solution that will rotate properly
//  this function should be used for all orientation change code in the app
function updateOrientation()
{
	var orientation = window.orientation;
	switch (orientation)
	{	
		case 0:			
			if (gameState == 'launch' || gameState == 'launchCleared')
			{
				showRotateNotice(gameState == 'launch'); // on launch, we need to set this because iPhone assumes all web app slaunch portrait
			}
			else
			{
				showMenu();
				showRotateNotice(false);
			}	
			break;
		case -90:
			if (gameState == 'launch')
				gameState = 'launchCleared';
			hideRotateNotice();
			break;
		case 90:
			if (gameState == 'launch')
				gameState = 'launchCleared';
			hideRotateNotice();
			break;
	}
}

function showRotateNotice(isLaunch)
{
	if (isLaunch)
	{
		noticeRotate.style.width = '320px';
		noticeRotate.style.height = '460px';
	}	
	else
	{
		noticeRotate.style.width = '480px';
		noticeRotate.style.height = '460px';
	}
	noticeRotate.style.display = 'block';
	setTimeout("noticeRotate.style.opacity = 1;", 1);	
}

function hideRotateNotice()
{
	noticeRotate.style.opacity = 0;
	setTimeout("noticeRotate.style.display = 'none'; ", 300);
}


/* !Font rendering */

const fontHeight = 7 * 2; // 2 for doubled pixels

function labelCounter(elementName, digits)
{
	this.digits = new Array();
	this.digitElements = new Array();

	container = document.getElementById(elementName);
	digit = null;
	this.digits = [];
	
	for (var i = 0; i < digits; i++)
	{
		digit = document.createElement('div');
		digit.id = elementName + '_digit_' + i;
		digit.className = 'labelDigit';
		
		container.appendChild(digit);
		this.digits[i] = 0; // set all to 0 by default
		this.digitElements[i] = digit;
	}
}


labelCounter.prototype.set = function(number)
{
	var nString = zeroPad(number, this.digits.length);
	
	for (var i = 0; i < this.digits.length; i++)
	{
		this.setDigit(i, parseInt(nString[i]));
	}
}

labelCounter.prototype.setDigit = function(digit, number)
{
	this.digitElements[digit].style.backgroundPositionY = fontHeight * number * -1; // -1 to move counter up one position
}

/* !Utility */

function zeroPad(num, length)
{
	var zeroPadded = num + '';	
	while(zeroPadded.length < length)
	{
		zeroPadded = "0" + zeroPadded;
	}
	return zeroPadded;
}

// ======================= Preloading ================================

requiredImages =
[
	"about-text.gif",
	"font1.gif",
	"game-over.gif",
	"guy1.gif",
	"guy2.gif",
	"guyFlip.gif",
	"chef0-1.gif",
	"chef0-2.gif",
	"chef0-flip.gif",
	"chef1-1.gif",
	"chef1-2.gif",
	"chef1-flip.gif",
	"life.gif",
	"map0.png",
	"map1.png",
	"menu-about.gif",
	"menu-about-active.gif",	
	"menu-about-small.png",
	"menu-about-small-active.png",
	"menu-back.gif",
	"menu-back-active.gif",
	"menu-new.gif",	
	"menu-new-active.gif",	
	"menu-continue.gif",	
	"menu-continue-active.gif",	
	"menu-rotate.gif",	
	"menuButton.gif",	
	"notice-time.gif",
	"notice-tokens.gif",
	"notice-total.gif",
	"bonus-score.gif",
	"pie-complete.gif",
	"scoreBar.gif",
	"token1.gif",	
	"token2.gif",	
	"token3.gif",
	"token4.gif"
]

// ImageLoader class
function ImageLoader(callback)
{
    this.m_callback     = callback;
    this.m_imagesLoaded = 0;
    this.m_imageUrls    = [];
    this.m_images       = [];
}

ImageLoader.prototype.setImageUrls = function(imageUrls)
{
    this.m_imagesLoaded = 0;
    this.m_imageUrls    = imageUrls;
    this.m_images       = new Array(imageUrls.length);
}

ImageLoader.prototype.imageUrls = function() { return this.m_imageUrls; }

ImageLoader.prototype.startLoading = function()
{   
    for (var i = 0; i < this.m_imageUrls.length; ++i)
    {
        var image = new Image();
        
        image.onload = function(image, i)
        {
            this._imageLoaded(image, i);
        }.bind(this, [image, i]);

        image.src = 'images/' + this.m_imageUrls[i];
    }
}

ImageLoader.prototype._imageLoaded = function(image, index)
{
    this.m_images[index] = image;

    ++this.m_imagesLoaded;
    	    
    if (this.m_imagesLoaded == this.m_imageUrls.length)
    {
        this.m_callback(this.m_images);
    }
}

Object.prototype.bind = function(object, args)
{
    var method = this;
    return function() { return method.apply(object, args); }
}

function copy(obj) {
    if (Object.prototype.toString.call(obj) === '[object Array]') {
        var out = [], i = 0, len = obj.length;
        for ( ; i < len; i++ ) {
            out[i] = arguments.callee(obj[i]);
        }
        return out;
    }
    if (typeof obj === 'object') {
        var out = {}, i;
        for ( i in obj ) {
            out[i] = arguments.callee(obj[i]);
        }
        return out;
    }
    return obj;
}

function logg(message)
{
	document.getElementById('logg').innerText += message + '<Br />';
}
function logAlert(message)
{
/* 	alert(message); */
}