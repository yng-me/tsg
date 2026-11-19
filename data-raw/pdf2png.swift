// Renders the first page of a PDF to a PNG at a given scale.
// Usage: swift data-raw/pdf2png.swift in.pdf out.png [scale]
import AppKit
import PDFKit

let args = CommandLine.arguments
let scale = args.count > 3 ? CGFloat(Double(args[3]) ?? 2) : 2
guard let doc = PDFDocument(url: URL(fileURLWithPath: args[1])), let page = doc.page(at: 0) else { exit(1) }
let box = page.bounds(for: .mediaBox)
let size = NSSize(width: box.width * scale, height: box.height * scale)
let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: Int(size.width), pixelsHigh: Int(size.height),
  bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
NSGraphicsContext.saveGraphicsState()
let ctx = NSGraphicsContext(bitmapImageRep: rep)!
NSGraphicsContext.current = ctx
ctx.cgContext.setFillColor(NSColor.white.cgColor)
ctx.cgContext.fill(CGRect(origin: .zero, size: size))
ctx.cgContext.scaleBy(x: scale, y: scale)
page.draw(with: .mediaBox, to: ctx.cgContext)
NSGraphicsContext.restoreGraphicsState()
try! rep.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: args[2]))
