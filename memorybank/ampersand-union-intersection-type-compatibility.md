# AMPERSAND UNION EN INTERSECTION TYPE COMPATIBILITEIT

## Geleerde les uit landeneisen interfaces fout

### Het probleem
In de interfaces kreeg ik een type error bij deze expressie:
```
(bronProductEis~\/bronOrganismeEis~\/bronProductOrganismeEis~\/bronDekking~\/bronVerpakkingEis~\/bronExterneReferentie~)
```

### De fout
```
Type error, cannot match:
  the concept VerpakkingEis (Tgt of bronVerpakkingEis [VerpakkingEis*Bron]~)
  and concept ExterneReferentie (Tgt of bronExterneReferentie [ExterneReferentie*Bron]~)
```

### De verklaring
De union operator (`\/`) en intersection operator (`/\`) in Ampersand werken als volgt:

- **Union (`\/`)**: Verenigt twee verzamelingen paren
- **Intersection (`/\`)**: Neemt de doorsnede van twee verzamelingen paren

**CRUCIALE REGEL**: Voor beide operatoren moeten ZOWEL de source- als target concepten van beide zijden identiek zijn.

### Type analyse van mijn foutieve code
```
bronProductEis~        : [ProductEis*Bron]
bronOrganismeEis~      : [OrganismeEis*Bron]  
bronProductOrganismeEis~ : [ProductOrganismeEis*Bron]
bronDekking~           : [Dekking*Bron]
bronVerpakkingEis~     : [VerpakkingEis*Bron]
bronExterneReferentie~ : [ExterneReferentie*Bron]
```

**Probleem**: Alle source concepten zijn verschillend (ProductEis, OrganismeEis, etc.), maar target concept (Bron) is wel hetzelfde. Voor union/intersection moeten BEIDE identiek zijn.

### De oplossing
In plaats van een union van verschillende type relaties, moet ik de interface splitsen in aparte delen voor elk type eis, of een andere benadering gebruiken.

### Algemene regel
**Type compatibility voor union/intersection**:
- Source concept linkerlid = Source concept rechterlid  
- Target concept linkerlid = Target concept rechterlid

Anders krijg je een type error.

### Toepassing
Dit verklaart waarom complexe union expressies vaak falen - Ampersand's type systeem is strikt en vereist exacte type matching voor set operaties.
