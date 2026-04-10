import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite(' Test d\'Intégration : Communication LSP', () => {

    test('Extraction directe de l\'AST via le Language Client', async function() {
    this.timeout(60000);

     const testFilePath = path.resolve(__dirname, '../../../../../L2/code/jlep/plugins/javacard/common/native/jcapi/src/mifare.c');
    //const testFilePath = path.resolve(__dirname, '../../../../test_files/test1.c');
const uri = vscode.Uri.file(testFilePath);
    const document = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(document);

    const extension = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
    await extension.activate();

    console.log(" Attente de l'initialisation du serveur OCaml...");
    await new Promise(resolve => setTimeout(resolve, 10000));

    console.log("--- DÉBUT DE LA RÉPONSE SERVEUR ---");
    try {
      
        
        const response = await vscode.commands.executeCommand('acsl-lsp.debugAST');
        
        if (response) {
            console.log(" RÉPONSE REÇUE :");
            console.log(JSON.stringify(response, null, 2));
        } else {
            console.log(" Le serveur a renvoyé 'undefined'.");

        }
    } catch (error) {
        console.error(" Erreur crash serveur:", error);
    }
    console.log("--- FIN DE LA RÉPONSE SERVEUR ---");
});
});


