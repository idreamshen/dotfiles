const canvas = document.getElementById('game-canvas');
const ctx = canvas.getContext('2d');
const scoreEl = document.getElementById('score');
const highScoreEl = document.getElementById('high-score');
const startBtn = document.getElementById('start-btn');
const pauseBtn = document.getElementById('pause-btn');

const GRID_SIZE = 20;
const CELL_SIZE = canvas.width / GRID_SIZE;

let snake, direction, nextDirection, food, score, highScore, gameLoop, gameState;

function init() {
  snake = [
    { x: 10, y: 10 },
    { x: 9, y: 10 },
    { x: 8, y: 10 },
  ];
  direction = { x: 1, y: 0 };
  nextDirection = { x: 1, y: 0 };
  score = 0;
  highScore = parseInt(localStorage.getItem('snake-high-score')) || 0;
  highScoreEl.textContent = highScore;
  gameState = 'idle';
  spawnFood();
  draw();
}

function spawnFood() {
  const free = [];
  for (let x = 0; x < GRID_SIZE; x++) {
    for (let y = 0; y < GRID_SIZE; y++) {
      if (!snake.some(s => s.x === x && s.y === y)) {
        free.push({ x, y });
      }
    }
  }
  food = free.length ? free[Math.floor(Math.random() * free.length)] : null;
}

function draw() {
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  ctx.fillStyle = '#0f3460';
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  ctx.fillStyle = '#112d50';
  for (let x = 0; x < GRID_SIZE; x++) {
    for (let y = 0; y < GRID_SIZE; y++) {
      if ((x + y) % 2 === 0) {
        ctx.fillRect(x * CELL_SIZE, y * CELL_SIZE, CELL_SIZE, CELL_SIZE);
      }
    }
  }

  snake.forEach((seg, i) => {
    ctx.fillStyle = i === 0 ? '#4ecca3' : '#36b37e';
    ctx.shadowColor = i === 0 ? '#4ecca3' : 'transparent';
    ctx.shadowBlur = i === 0 ? 8 : 0;
    ctx.beginPath();
    ctx.roundRect(seg.x * CELL_SIZE + 1, seg.y * CELL_SIZE + 1, CELL_SIZE - 2, CELL_SIZE - 2, 4);
    ctx.fill();
  });
  ctx.shadowBlur = 0;

  if (food) {
    ctx.fillStyle = '#e94560';
    ctx.shadowColor = '#e94560';
    ctx.shadowBlur = 10;
    ctx.beginPath();
    ctx.arc(
      food.x * CELL_SIZE + CELL_SIZE / 2,
      food.y * CELL_SIZE + CELL_SIZE / 2,
      CELL_SIZE / 2 - 2,
      0,
      Math.PI * 2
    );
    ctx.fill();
    ctx.shadowBlur = 0;
  }
}

function step() {
  if (gameState !== 'playing') return;

  direction = { ...nextDirection };
  const head = {
    x: snake[0].x + direction.x,
    y: snake[0].y + direction.y,
  };

  if (head.x < 0 || head.x >= GRID_SIZE || head.y < 0 || head.y >= GRID_SIZE) {
    gameOver();
    return;
  }

  if (snake.some(s => s.x === head.x && s.y === head.y)) {
    gameOver();
    return;
  }

  snake.unshift(head);

  if (food && head.x === food.x && head.y === food.y) {
    score++;
    scoreEl.textContent = score;
    spawnFood();
    if (!food) gameOver();
  } else {
    snake.pop();
  }

  draw();
}

function gameOver() {
  gameState = 'idle';
  clearInterval(gameLoop);
  gameLoop = null;
  if (score > highScore) {
    highScore = score;
    localStorage.setItem('snake-high-score', highScore);
    highScoreEl.textContent = highScore;
  }

  ctx.fillStyle = 'rgba(0,0,0,0.65)';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = '#fff';
  ctx.font = 'bold 28px -apple-system, sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('游戏结束', canvas.width / 2, canvas.height / 2 - 10);
  ctx.font = '16px -apple-system, sans-serif';
  ctx.fillText(`得分: ${score}`, canvas.width / 2, canvas.height / 2 + 24);

  startBtn.textContent = '重新开始';
  pauseBtn.disabled = true;
}

function startGame() {
  if (gameLoop) {
    clearInterval(gameLoop);
    gameLoop = null;
  }
  init();
  gameState = 'playing';
  gameLoop = setInterval(step, 120);
  startBtn.textContent = '重新开始';
  pauseBtn.disabled = false;
  pauseBtn.textContent = '暂停';
}

function togglePause() {
  if (gameState === 'playing') {
    gameState = 'paused';
    pauseBtn.textContent = '继续';
  } else if (gameState === 'paused') {
    gameState = 'playing';
    pauseBtn.textContent = '暂停';
  }
}

function setDirection(dx, dy) {
  if (direction.x + dx === 0 && direction.y + dy === 0) return;
  nextDirection = { x: dx, y: dy };
}

startBtn.addEventListener('click', startGame);
pauseBtn.addEventListener('click', togglePause);

document.addEventListener('keydown', e => {
  if (['ArrowUp', 'w', 'W'].includes(e.key)) { e.preventDefault(); setDirection(0, -1); }
  if (['ArrowDown', 's', 'S'].includes(e.key)) { e.preventDefault(); setDirection(0, 1); }
  if (['ArrowLeft', 'a', 'A'].includes(e.key)) { e.preventDefault(); setDirection(-1, 0); }
  if (['ArrowRight', 'd', 'D'].includes(e.key)) { e.preventDefault(); setDirection(1, 0); }
  if (e.key === ' ' || e.key === 'Escape') {
    e.preventDefault();
    if (gameState === 'playing' || gameState === 'paused') togglePause();
  }
});

document.getElementById('btn-up').addEventListener('click', () => setDirection(0, -1));
document.getElementById('btn-down').addEventListener('click', () => setDirection(0, 1));
document.getElementById('btn-left').addEventListener('click', () => setDirection(-1, 0));
document.getElementById('btn-right').addEventListener('click', () => setDirection(1, 0));

let touchStartX = 0, touchStartY = 0;
canvas.addEventListener('touchstart', e => {
  const t = e.touches[0];
  touchStartX = t.clientX;
  touchStartY = t.clientY;
});
canvas.addEventListener('touchmove', e => e.preventDefault());
canvas.addEventListener('touchend', e => {
  const rect = canvas.getBoundingClientRect();
  const endX = touchStartX + (e.changedTouches[0].clientX - touchStartX);
  const endY = touchStartY + (e.changedTouches[0].clientY - touchStartY);
  const dx = endX - touchStartX;
  const dy = endY - touchStartY;
  if (Math.abs(dx) > Math.abs(dy)) {
    setDirection(dx > 0 ? 1 : -1, 0);
  } else {
    setDirection(0, dy > 0 ? 1 : -1);
  }
});

init();
