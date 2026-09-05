# Cloudflare Pages edge frontend

This Pages Worker proxies the public dashboard to the Cloud Run R Shiny service.

- Public URL: `https://cartera-autos.gonor.me`
- Backend: `https://cartera-autos-d3qj5vwxtq-uc.a.run.app`

Deploy directly with:

```bash
wrangler pages deploy cloudflare-pages --project-name cartera-autos --branch main
```

The Pages project custom domain is managed through the Cloudflare Pages API.
