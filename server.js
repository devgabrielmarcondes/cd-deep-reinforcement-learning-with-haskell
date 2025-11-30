const http = require('http');
const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

const PORT = 3000;
const WEB_ROOT = path.join(__dirname, 'web_application', 'stitch', 'projects', '3d_maze');

const mimeTypes = {
    '.html': 'text/html',
    '.js': 'text/javascript',
    '.css': 'text/css',
    '.json': 'application/json',
};

const server = http.createServer((req, res) => {
    console.log(`${req.method} ${req.url}`);

    // Configuração CORS
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

    // Tratamento de preflight request (OPTIONS)
    if (req.method === 'OPTIONS') {
        res.writeHead(204);
        res.end();
        return;
    }

    // Endpoint para rodar o Haskell
    if (req.method === 'POST' && req.url === '/run') {
        let body = '';
        req.on('data', chunk => {
            body += chunk.toString();
        });
        req.on('end', () => {
            const { difficulty } = JSON.parse(body);
            console.log(`Executando Haskell com dificuldade: ${difficulty}`);

            // Mapeia dificuldade string para número esperado pelo Haskell
            const diffMap = { 'Easy': '1', 'Medium': '2', 'Hard': '3' };
            const inputVal = diffMap[difficulty] || '1';

            // Executa o script Haskell
            // Assumindo que 'runhaskell' está no PATH. Se não, pode precisar compilar antes.
            const child = spawn('runhaskell', ['DeepRL.hs'], { cwd: __dirname });

            // Envia a dificuldade para o stdin do processo Haskell
            child.stdin.write(inputVal + '\n');
            child.stdin.end();

            let output = '';

            child.stdout.on('data', (data) => {
                output += data.toString();
                // console.log(`Haskell stdout: ${data}`);
            });

            child.stderr.on('data', (data) => {
                console.error(`Haskell stderr: ${data}`);
            });

            child.on('close', (code) => {
                console.log(`Processo Haskell terminou com código ${code}`);
                if (code === 0) {
                    res.writeHead(200, { 'Content-Type': 'application/json' });
                    res.end(JSON.stringify({ success: true, message: 'Labirinto gerado com sucesso!' }));
                } else {
                    res.writeHead(500, { 'Content-Type': 'application/json' });
                    res.end(JSON.stringify({ success: false, message: 'Erro ao executar Haskell.' }));
                }
            });
        });
        return;
    }

    // Servir maze_data.json da raiz
    if (req.url.startsWith('/maze_data.json')) {
        const filePath = path.join(__dirname, 'maze_data.json');
        serveFile(res, filePath);
        return;
    }

    // Servir arquivos estáticos
    let filePath = path.join(WEB_ROOT, req.url === '/' ? 'index.html' : req.url);

    // Segurança simples para não sair da pasta permitida (exceto maze_data.json tratado acima)
    if (!filePath.startsWith(WEB_ROOT) && req.url !== '/') {
        res.writeHead(403);
        res.end('Forbidden');
        return;
    }

    serveFile(res, filePath);
});

function serveFile(res, filePath) {
    const ext = path.extname(filePath);
    const contentType = mimeTypes[ext] || 'application/octet-stream';

    fs.readFile(filePath, (err, content) => {
        if (err) {
            if (err.code === 'ENOENT') {
                res.writeHead(404);
                res.end('File not found');
            } else {
                res.writeHead(500);
                res.end(`Server Error: ${err.code}`);
            }
        } else {
            res.writeHead(200, { 'Content-Type': contentType });
            res.end(content);
        }
    });
}

server.listen(PORT, () => {
    console.log(`Servidor rodando em http://localhost:${PORT}`);
    console.log(`Abra o navegador para usar a aplicação.`);
});
