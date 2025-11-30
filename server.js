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

let currentLogs = '';

const server = http.createServer((req, res) => {
    console.log(`${req.method} ${req.url}`);

    // Parse URL para separar path de query params
    const parsedUrl = new URL(req.url, 'http://localhost');
    const pathname = parsedUrl.pathname;

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
    if (req.method === 'POST' && pathname === '/run') {
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

            // Limpa logs em memória
            currentLogs = 'Iniciando execução...\n';

            // Executa o script Haskell
            const child = spawn('runhaskell', ['DeepRL.hs'], { cwd: __dirname });

            child.on('error', (err) => {
                console.error('Falha ao iniciar subprocesso:', err);
                currentLogs += `\nERRO CRÍTICO: Falha ao iniciar 'runhaskell'. Verifique se o GHC está instalado e no PATH.\n${err.message}`;
                res.writeHead(500, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify({ success: false, message: 'Falha ao iniciar processo Haskell.' }));
            });

            // Envia a dificuldade para o stdin do processo Haskell
            if (child.stdin) {
                child.stdin.write(inputVal + '\n');
                child.stdin.end();
            }

            child.stdout.on('data', (data) => {
                const str = data.toString();
                currentLogs += str;
            });

            child.stderr.on('data', (data) => {
                const str = data.toString();
                console.error(`Haskell stderr: ${data}`);
                currentLogs += `ERROR: ${str}`;
            });

            child.on('close', (code) => {
                console.log(`Processo Haskell terminou com código ${code}`);
                if (code === 0) {
                    // Verifica se a resposta já não foi enviada (em caso de erro de spawn)
                    if (!res.writableEnded) {
                        res.writeHead(200, { 'Content-Type': 'application/json' });
                        res.end(JSON.stringify({ success: true, message: 'Labirinto gerado com sucesso!' }));
                    }
                } else {
                    if (!res.writableEnded) {
                        res.writeHead(500, { 'Content-Type': 'application/json' });
                        res.end(JSON.stringify({ success: false, message: 'Erro ao executar Haskell.' }));
                    }
                }
            });
        });
        return;
    }

    // Servir logs da memória
    if (pathname === '/logs.txt') {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end(currentLogs);
        return;
    }

    // Servir maze_data.json da raiz
    if (pathname === '/maze_data.json') {
        const filePath = path.join(__dirname, 'maze_data.json');
        serveFile(res, filePath);
        return;
    }

    // Servir arquivos estáticos
    let filePath = path.join(WEB_ROOT, pathname === '/' ? 'index.html' : pathname);

    // Segurança simples para não sair da pasta permitida
    if (!filePath.startsWith(WEB_ROOT) && pathname !== '/') {
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
            console.error(`Erro ao servir arquivo ${filePath}:`, err);
            if (err.code === 'ENOENT') {
                res.writeHead(404);
                res.end('File not found');
            } else {
                res.writeHead(500);
                res.end(`Server Error: ${err.code}`);
            }
        } else {
            console.log(`Servindo arquivo: ${filePath} (${content.length} bytes)`);
            res.writeHead(200, { 'Content-Type': contentType });
            res.end(content);
        }
    });
}

server.listen(PORT, () => {
    console.log(`Servidor rodando em http://localhost:${PORT}`);
    console.log(`Abra o navegador para usar a aplicação.`);
});
