import { commands, ExtensionContext, EventEmitter, Range, Selection, TextDocumentContentProvider, TextEditor, TextEditorRevealType, TextEditorSelectionChangeEvent, Uri, ViewColumn, workspace, window } from 'vscode';
import { NotificationType } from 'vscode-jsonrpc';
import { LanguageClient, DidChangeConfigurationNotification } from 'vscode-languageclient';

export namespace Monto {

    // Products

    export interface Product {
        uri: string;
        name: string;
        language: string;
        content: string;
        rangeMap: RangeEntry[];
        rangeMapRev: RangeEntry[];

        // Internal fields
        handleSelectionChange: boolean;
    }

    export interface RangeEntry {
        source: OffsetRange;
        targets: OffsetRange[];
    }

    export interface OffsetRange {
        start: number; end: number;
    }

    namespace PublishProduct {
        export const type = new NotificationType<Product, void>(
            "monto/publishProduct"
        );
    }

    // Map Monto uri strings to latest version of their products
    let products = new Map<string, Product>();

    // Map all uri strings to the view column in which they are displayed
    let columns = new Map<string, ViewColumn>();

    function saveProduct(product: Product) {
        let uri = productToTargetUri(product);
        products.set(uri.toString(), product);
        product.handleSelectionChange = false;
        montoProvider.onDidChangeEmitter.fire(uri);
    }

    export function showProduct(product: Product) {
        openInEditor(productToTargetUri(product), true);
    }

    function getProduct(uri: Uri): Product {
        let p = products.get(uri.toString());
        if (p === undefined) {
            let dummyRange = {
                source: { start: 0, end: 0 },
                targets: [{ start: 0, end: 0 }]
            };
            return {
                uri: "",
                name: "",
                language: "",
                content: "",
                rangeMap: [dummyRange],
                rangeMapRev: [dummyRange],
                handleSelectionChange: false
            };
        } else {
            return p;
        }
    }

    function productToTargetUri(product: Product): Uri {
        let path = Uri.parse(product.uri).path;
        return Uri.parse(`monto:${path}-${product.name}.${product.language}`);
    }

    function targetUriToSourceUri(uri: Uri): Uri {
        let path = uri.path.substring(0, uri.path.lastIndexOf("-"));
        return Uri.parse(`file:${path}`);
    }

    // Monto URI scheme

    const montoScheme = 'monto';

    const montoProvider = new class implements TextDocumentContentProvider {
        onDidChangeEmitter = new EventEmitter<Uri>();

        provideTextDocumentContent(uri: Uri): string {
            let product = products.get(uri.toString());
            if (product === undefined) {
                return "unknown content";
            } else {
                return product.content;
            }
        }

        get onDidChange() {
            return this.onDidChangeEmitter.event;
        }

        dispose() {
            this.onDidChangeEmitter.dispose();
        }
    };

    // Setup

    export function setup(
            name: string,
            context: ExtensionContext,
            client: LanguageClient
        ) {
        window.onDidChangeTextEditorSelection(change => {
            if (isMontoEditor(change.textEditor)) {
                selectLinkedSourceRanges(change);
            }
        });

        window.onDidChangeVisibleTextEditors(editors => {
            editors.forEach(editor => {
                if (editor.viewColumn !== undefined) {
                    columns.set(editor.document.uri.toString(), editor.viewColumn);
                }
            });
        });

        workspace.onDidChangeConfiguration(event => {
            if (event.affectsConfiguration(name)) {
                sendConfigurationToServer(client, name);
            }
        });

        context.subscriptions.push(
            commands.registerCommand(`${name}.selectLinkedEditors`, () => {
                selectLinkedTargetRanges();
            })
        );

        context.subscriptions.push(workspace.registerTextDocumentContentProvider(montoScheme, montoProvider));

        client.clientOptions.initializationOptions = workspace.getConfiguration(name);

        client.onReady().then(_ => {
            client.onNotification(PublishProduct.type, product => {
                saveProduct(product);
                showProduct(product);
            });
        });
    }

    function sendConfigurationToServer(client: LanguageClient, name: string) {
        client.sendNotification(
            DidChangeConfigurationNotification.type.method,
            { settings: workspace.getConfiguration(name) }
        );
    }

    function isMontoEditor(editor: TextEditor): Boolean {
        return editor.document.uri.scheme === 'monto';
    }

    // Source to target linking

    function selectLinkedTargetRanges() {
        let editor = window.activeTextEditor;
        if (editor !== undefined) {
            let sourceEditor = editor;
            let sourceUri = sourceEditor.document.uri.toString();
            let sourceSelections = sourceEditor.selections;
            window.visibleTextEditors.forEach(targetEditor => {
                if (isMontoEditor(targetEditor)) {
                    let targetUri = targetEditor.document.uri;
                    let targetSourceUri = targetUriToSourceUri(targetUri);
                    if (targetSourceUri.toString() === sourceUri) {
                        let product = getProduct(targetUri);
                        let targetSelections =
                            flatten(sourceSelections.map(sourceSelection => {
                                return getSelections(product, sourceEditor, sourceSelection, targetEditor, true);
                            }));
                        if (targetSelections.length > 0) {
                            product.handleSelectionChange = false;
                            showSelections(targetUri, targetEditor, targetSelections, true);
                        }
                    }
                }
            });
        }
    }

    // Target to source linking

    function selectLinkedSourceRanges(change: TextEditorSelectionChangeEvent) {
        let targetEditor = change.textEditor;
        let targetUri = targetEditor.document.uri;
        let sourceUri = targetUriToSourceUri(targetUri);
        openInEditor(sourceUri, false).then(sourceEditor => {
            let product = getProduct(targetUri);
            if (product.handleSelectionChange) {
                let sourceSelections =
                    flatten(change.selections.map(targetSelection => {
                        let x = getSelections(product, targetEditor, targetSelection, sourceEditor, false);
                        return x;
                    }));
                if (sourceSelections.length > 0) {
                    showSelections(sourceUri, sourceEditor, sourceSelections, false);
                }
            } else {
                product.handleSelectionChange = true;
            }
        });
    }

    // Utilities

    function flatten(ranges: Range[][]): Range[] {
        return ranges.reduce((a, b) => a.concat(b));
    }

    function getSelections(product : Product, fromEditor: TextEditor, fromSelection: Selection, toEditor: TextEditor, forward: boolean): Range[] {
        let fromOffset = fromEditor.document.offsetAt(fromSelection.start);
        let entry = findContainingRangeEntry(product, fromOffset, forward);
        if (entry === undefined) {
            return [new Range(0, 0, 0, 0)];
        } else {
            return targetsToSelections(toEditor, entry.targets);
        }
    }

    function findContainingRangeEntry(product: Product, offset: number, forward: boolean): RangeEntry| undefined {
        let map = forward ? product.rangeMap : product.rangeMapRev;
        return map.find(entry =>
            (entry.source.start <= offset) && (offset < entry.source.end)
        );
    }

    function targetsToSelections(editor: TextEditor, targets: OffsetRange[]): Range[] {
        return targets.map(target => {
            return targetToSelection(editor, target);
        });
    }

    function targetToSelection(editor: TextEditor, target: OffsetRange): Range {
        let s = editor.document.positionAt(target.start);
        let f = editor.document.positionAt(target.end);
        return new Range(s, f);
    }

    function viewColumn(uri: Uri, isTarget: Boolean): ViewColumn {
        let key = uri.toString();
        let column = columns.get(key);
        if (column === undefined) {
            let original = isTarget ? ViewColumn.Two : ViewColumn.One;
            columns.set(key, original);
            return original;
        } else {
            return column;
        }
    }

    function showSelections(uri : Uri, editor: TextEditor, selections: Range[], isTarget: Boolean) {
        window.showTextDocument(
            editor.document,
            {
                preserveFocus: false,
                preview: false,
                viewColumn: viewColumn(uri, isTarget)
            }
        );
        editor.selections = selections.map(s => new Selection(s.start, s.end));
        editor.revealRange(selections[0], TextEditorRevealType.InCenterIfOutsideViewport);
    }

    function openInEditor(uri: Uri, isTarget: boolean): Thenable<TextEditor> {
        return window.showTextDocument(
            uri,
            {
                preserveFocus: true,
                preview: false,
                viewColumn: viewColumn(uri, isTarget)
            }
        );
    }

}
