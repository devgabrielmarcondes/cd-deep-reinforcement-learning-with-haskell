// ==========================================
// 1. Configurações e Tipos
// ==========================================

const Configs = {
    Easy:   { rows: 5,  cols: 5,  wallProb: 0.15, epochs: 300 },
    Medium: { rows: 7,  cols: 7,  wallProb: 0.25, epochs: 800 },
    Hard:   { rows: 10, cols: 10, wallProb: 0.30, epochs: 3000 }
};

// ==========================================
// 2. Lógica do Labirinto
// ==========================================

function isValidBounds(r, c, rows, cols) {
    return r >= 0 && r < rows && c >= 0 && c < cols;
}

function hasPath(maze, start, end) {
    const rows = maze.length;
    const cols = maze[0].length;
    const queue = [start];
    const visited = new Set();
    visited.add(`${start.r},${start.c}`);

    const dirs = [[0, 1], [0, -1], [1, 0], [-1, 0]];

    while (queue.length > 0) {
        const curr = queue.shift();
        if (curr.r === end.r && curr.c === end.c) return true;

        for (const [dr, dc] of dirs) {
            const nr = curr.r + dr;
            const nc = curr.c + dc;
            const key = `${nr},${nc}`;

            if (isValidBounds(nr, nc, rows, cols) && 
                maze[nr][nc] !== '#' && 
                !visited.has(key)) {
                visited.add(key);
                queue.push({ r: nr, c: nc });
            }
        }
    }
    return false;
}

function generateRandomGrid(config) {
    const { rows, cols, wallProb } = config;
    let maze = [];
    for (let r = 0; r < rows; r++) {
        let row = [];
        for (let c = 0; c < cols; c++) {
            row.push(Math.random() < wallProb ? '#' : ' ');
        }
        maze.push(row);
    }
    // Force Start and End
    maze[0][0] = 'P';
    maze[rows - 1][cols - 1] = 'X';
    return maze;
}

function generateSolvableMaze(config) {
    let maze;
    let attempts = 0;
    do {
        maze = generateRandomGrid(config);
        attempts++;
    } while (!hasPath(maze, { r: 0, c: 0 }, { r: config.rows - 1, c: config.cols - 1 }));
    console.log(`Maze generated in ${attempts} attempts.`);
    return maze;
}

// ==========================================
// 3. Lógica do Ambiente (RL)
// ==========================================

function step(maze, pos, action) {
    const rows = maze.length;
    const cols = maze[0].length;
    const currentCell = maze[pos.r][pos.c];

    if (currentCell === 'X') {
        return { newPos: pos, reward: 0, done: true };
    }

    let nr = pos.r;
    let nc = pos.c;

    // 0: Up, 1: Down, 2: Left, 3: Right
    if (action === 0) nr -= 1;
    else if (action === 1) nr += 1;
    else if (action === 2) nc -= 1;
    else if (action === 3) nc += 1;

    const valid = isValidBounds(nr, nc, rows, cols) && maze[nr][nc] !== '#';
    const newPos = valid ? { r: nr, c: nc } : pos;
    const newCell = maze[newPos.r][newPos.c];
    const done = newCell === 'X';

    let reward;
    if (newCell === 'X') reward = 100.0;
    else if (!valid) reward = -5.0;
    else reward = -1.0;

    return { newPos, reward, done };
}

// ==========================================
// 4. Treinamento (Q-Learning)
// ==========================================

function initWeights(rows, cols) {
    let w = [];
    for (let r = 0; r < rows; r++) {
        let row = [];
        for (let c = 0; c < cols; c++) {
            row.push([0, 0, 0, 0]); // 4 actions
        }
        w.push(row);
    }
    return w;
}

function chooseAction(weights, pos, epsilon) {
    if (Math.random() < epsilon) {
        return Math.floor(Math.random() * 4);
    } else {
        const qs = weights[pos.r][pos.c];
        // Find max index
        let maxVal = -Infinity;
        let maxActions = [];
        for (let i = 0; i < 4; i++) {
            if (qs[i] > maxVal) {
                maxVal = qs[i];
                maxActions = [i];
            } else if (qs[i] === maxVal) {
                maxActions.push(i);
            }
        }
        return maxActions[Math.floor(Math.random() * maxActions.length)];
    }
}

function trainLoop(maze, config) {
    const { rows, cols, epochs } = config;
    const weights = initWeights(rows, cols);
    const gamma = 0.9;
    const alpha = 0.1;

    for (let epoch = 1; epoch <= epochs; epoch++) {
        let progress = epoch / epochs;
        let epsilon = Math.max(0.05, 1.0 - progress);
        
        let pos = { r: 0, c: 0 };
        let steps = 0;
        const maxSteps = rows * cols * 3;

        while (steps < maxSteps) {
            const action = chooseAction(weights, pos, epsilon);
            const { newPos, reward, done } = step(maze, pos, action);

            const qCurr = weights[pos.r][pos.c][action];
            const qNextMax = Math.max(...weights[newPos.r][newPos.c]);
            
            const target = reward + gamma * qNextMax;
            weights[pos.r][pos.c][action] = qCurr + alpha * (target - qCurr);

            pos = newPos;
            steps++;
            if (done) break;
        }
    }
    return weights;
}

function solveMaze(maze, weights) {
    let path = [];
    let pos = { r: 0, c: 0 };
    path.push({ ...pos });
    
    let steps = 0;
    const maxSteps = 100; // Safety break

    while (steps < maxSteps) {
        const action = chooseAction(weights, pos, 0.0); // Epsilon 0
        const { newPos, done } = step(maze, pos, action);
        
        // Avoid infinite loops in visualization if agent gets stuck (shouldn't happen if trained well)
        if (newPos.r === pos.r && newPos.c === pos.c) {
             // Hit wall, but we record it to show "bonk" maybe? 
             // For now, just ignore or break if stuck
        }
        
        pos = newPos;
        path.push({ ...pos });
        steps++;
        if (done) break;
    }
    return path;
}

// ==========================================
// 5. Three.js Visualization
// ==========================================

let scene, camera, renderer;
let mazeGroup;
let agentMesh;
let animationPath = [];
let animationIndex = 0;
let animationSpeed = 300; // ms per step
let lastStepTime = 0;
let isAnimating = false;

function initThree() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x111111); // Dark background

    camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.set(0, 10, 10);
    camera.lookAt(0, 0, 0);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(window.innerWidth, window.innerHeight);
    renderer.shadowMap.enabled = true;
    document.body.appendChild(renderer.domElement);

    // Lights
    const ambientLight = new THREE.AmbientLight(0x404040);
    scene.add(ambientLight);

    const dirLight = new THREE.DirectionalLight(0xffffff, 0.5);
    dirLight.position.set(5, 10, 5);
    dirLight.castShadow = true;
    scene.add(dirLight);

    window.addEventListener('resize', onWindowResize, false);
    animate();
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}

function createMazeMesh(maze) {
    if (mazeGroup) scene.remove(mazeGroup);
    mazeGroup = new THREE.Group();

    const rows = maze.length;
    const cols = maze[0].length;

    // Center the maze
    const offsetX = (cols - 1) / 2;
    const offsetZ = (rows - 1) / 2;

    // Floor
    const floorGeo = new THREE.PlaneGeometry(cols, rows);
    const floorMat = new THREE.MeshStandardMaterial({ color: 0x222222, roughness: 0.8 });
    const floor = new THREE.Mesh(floorGeo, floorMat);
    floor.rotation.x = -Math.PI / 2;
    floor.receiveShadow = true;
    mazeGroup.add(floor);

    // Walls
    const wallGeo = new THREE.BoxGeometry(1, 1, 1);
    const wallMat = new THREE.MeshStandardMaterial({ color: 0x444444 });

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            const cell = maze[r][c];
            const x = c - offsetX;
            const z = r - offsetZ;

            if (cell === '#') {
                const wall = new THREE.Mesh(wallGeo, wallMat);
                wall.position.set(x, 0.5, z);
                wall.castShadow = true;
                wall.receiveShadow = true;
                mazeGroup.add(wall);
            } else if (cell === 'P') {
                // Start Marker (Green Light)
                const light = new THREE.PointLight(0x00ff00, 1, 5);
                light.position.set(x, 1, z);
                mazeGroup.add(light);
                
                const markerGeo = new THREE.SphereGeometry(0.2);
                const markerMat = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
                const marker = new THREE.Mesh(markerGeo, markerMat);
                marker.position.set(x, 0.2, z);
                mazeGroup.add(marker);
            } else if (cell === 'X') {
                // End Marker (Red Light)
                const light = new THREE.PointLight(0xff0000, 1, 5);
                light.position.set(x, 1, z);
                mazeGroup.add(light);

                const markerGeo = new THREE.SphereGeometry(0.2);
                const markerMat = new THREE.MeshBasicMaterial({ color: 0xff0000 });
                const marker = new THREE.Mesh(markerGeo, markerMat);
                marker.position.set(x, 0.2, z);
                mazeGroup.add(marker);
            }
        }
    }

    // Agent
    const agentGeo = new THREE.SphereGeometry(0.3);
    const agentMat = new THREE.MeshStandardMaterial({ color: 0x00ffff, emissive: 0x004444 });
    agentMesh = new THREE.Mesh(agentGeo, agentMat);
    // Start pos
    const startX = 0 - offsetX;
    const startZ = 0 - offsetZ;
    agentMesh.position.set(startX, 0.3, startZ);
    agentMesh.castShadow = true;
    mazeGroup.add(agentMesh);

    scene.add(mazeGroup);
    
    // Adjust camera
    const maxDim = Math.max(rows, cols);
    camera.position.set(0, maxDim * 1.2, maxDim * 0.8);
    camera.lookAt(0, 0, 0);
}

function animate(time) {
    requestAnimationFrame(animate);

    if (isAnimating && animationPath.length > 0) {
        if (time - lastStepTime > animationSpeed) {
            lastStepTime = time;
            
            if (animationIndex < animationPath.length) {
                const pos = animationPath[animationIndex];
                // Convert grid pos to world pos
                // We need to know rows/cols again, or store offsets.
                // Let's recalculate offsets based on mazeGroup children or store them globally.
                // Hack: we know the maze size from the current config.
                const diffStr = document.getElementById('difficulty').value;
                const conf = Configs[diffStr];
                const offsetX = (conf.cols - 1) / 2;
                const offsetZ = (conf.rows - 1) / 2;

                const x = pos.c - offsetX;
                const z = pos.r - offsetZ;
                
                agentMesh.position.set(x, 0.3, z);
                
                // Add trail (Yellow dots)
                const trailGeo = new THREE.SphereGeometry(0.1);
                const trailMat = new THREE.MeshBasicMaterial({ color: 0xffff00 });
                const trail = new THREE.Mesh(trailGeo, trailMat);
                trail.position.set(x, 0.1, z);
                mazeGroup.add(trail);

                animationIndex++;
            } else {
                isAnimating = false;
                document.getElementById('status').innerText = "Chegou ao destino!";
            }
        }
    }

    renderer.render(scene, camera);
}

// ==========================================
// 6. Integração UI
// ==========================================

document.getElementById('start-btn').addEventListener('click', () => {
    const diffStr = document.getElementById('difficulty').value;
    const config = Configs[diffStr];
    
    document.getElementById('status').innerText = "Gerando Labirinto...";
    
    // Use setTimeout to allow UI to update
    setTimeout(() => {
        const maze = generateSolvableMaze(config);
        createMazeMesh(maze);
        
        document.getElementById('status').innerText = `Treinando (${config.epochs} épocas)...`;
        
        setTimeout(() => {
            const weights = trainLoop(maze, config);
            const path = solveMaze(maze, weights);
            
            document.getElementById('status').innerText = `Solução encontrada: ${path.length} passos. Animando...`;
            document.getElementById('stats').innerText = `Passos: ${path.length}`;
            
            animationPath = path;
            animationIndex = 0;
            isAnimating = true;
        }, 100);
    }, 100);
});

// Init
initThree();
