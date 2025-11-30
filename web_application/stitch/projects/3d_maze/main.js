// ==========================================
// 1. Configurações e Tipos
// ==========================================

// Configs removidas pois a lógica agora é controlada pelo Haskell.
// O frontend apenas visualiza o que recebe no JSON.

// ==========================================
// 2. Three.js Visualization
// ==========================================

let scene, camera, renderer, controls;
let mazeGroup;
let agentMesh;
let animationPath = [];
let animationIndex = 0;
let animationSpeed = 300; // ms per step
let lastStepTime = 0;
let isAnimating = false;
let currentMazeDims = { rows: 0, cols: 0 };

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

    controls = new THREE.OrbitControls(camera, renderer.domElement);
    controls.enableDamping = true; // an animation loop is required when either damping or auto-rotation are enabled
    controls.dampingFactor = 0.05;
    controls.screenSpacePanning = false;
    controls.minDistance = 10;
    controls.maxDistance = 50;
    controls.maxPolarAngle = Math.PI / 2;

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

    currentMazeDims = { rows, cols };

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

    // Reset controls target to center
    if (controls) {
        controls.target.set(0, 0, 0);
        controls.update();
    }
}

function animate(time) {
    requestAnimationFrame(animate);

    if (controls) controls.update();

    if (isAnimating && animationPath.length > 0) {
        if (time - lastStepTime > animationSpeed) {
            lastStepTime = time;

            if (animationIndex < animationPath.length) {
                const pos = animationPath[animationIndex];

                if (pos && typeof pos.r === 'number' && typeof pos.c === 'number') {
                    // Convert grid pos to world pos
                    const offsetX = (currentMazeDims.cols - 1) / 2;
                    const offsetZ = (currentMazeDims.rows - 1) / 2;

                    const x = pos.c - offsetX;
                    const z = pos.r - offsetZ;

                    if (agentMesh) agentMesh.position.set(x, 0.3, z);

                    // Add trail (Yellow dots)
                    const trailGeo = new THREE.SphereGeometry(0.1);
                    const trailMat = new THREE.MeshBasicMaterial({ color: 0xffff00 });
                    const trail = new THREE.Mesh(trailGeo, trailMat);
                    trail.position.set(x, 0.1, z);
                    if (mazeGroup) mazeGroup.add(trail);
                }

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
// 3. Integração UI
// ==========================================

document.getElementById('start-btn').addEventListener('click', () => {
    const diffStr = document.getElementById('difficulty').value;
    document.getElementById('status').innerText = "Executando Haskell (pode demorar)...";

    // Determina a URL base da API
    // Se estiver rodando na porta 3000 (nosso servidor Node), usa caminho relativo.
    // Se estiver rodando em outra porta (ex: Live Server 5500), usa localhost:3000.
    const API_BASE_URL = window.location.port === '3000' ? '' : 'http://localhost:3000';
    console.log("API URL:", API_BASE_URL);

    // 1. Chama o servidor para rodar o Haskell
    fetch(`${API_BASE_URL}/run`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ difficulty: diffStr })
    })
        .then(response => {
            if (!response.ok) throw new Error("Erro ao executar o backend Haskell.");
            return response.json();
        })
        .then(runData => {
            if (!runData.success) throw new Error(runData.message);

            document.getElementById('status').innerText = "Carregando dados...";

            // 2. Carrega o JSON gerado
            const timestamp = new Date().getTime();
            const jsonUrl = `${API_BASE_URL}/maze_data.json?t=${timestamp}`;
            console.log("Fetching JSON from:", jsonUrl);
            return fetch(jsonUrl);
        })
        .then(response => {
            if (!response.ok) throw new Error("Erro ao carregar maze_data.json.");
            return response.json();
        })
        .then(data => {
            const maze = data.maze;
            const path = data.path;

            createMazeMesh(maze);

            document.getElementById('status').innerText = `Solução carregada: ${path.length} passos. Animando...`;
            document.getElementById('stats').innerText = `Passos: ${path.length}`;

            animationPath = path;
            animationIndex = 0;
            isAnimating = true;
        })
        .catch(err => {
            console.error(err);
            document.getElementById('status').innerText = "Erro: " + err.message;
        });
});// Init
initThree();